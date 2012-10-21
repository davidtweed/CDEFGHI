

import Control.Monad (liftM)
import Data.Char
import Language.Haskell.Extension (Extension)
import Maybe

-- -----------------------------------------------------------------------------

isGenAlpha c=(c>='a' && c<='z') || (c>='A' && c<='Z') || c=='_' || c=='%'
isGenAlphaNum c=isGenAlpha c || (c>='0' && c<='9')

type Type=String
-- Arr Entity IndexesString "isConceptual"
data ArrExp=Arr String [String] Bool deriving (Eq,Show)
--type Asgn=(Asgn',OptTy) -- assignment with optional LHS type
data Op=Op String OptTy deriving (Eq,Show) -- operator with type it occurs in
type RHSArrExp=Either ArrExp String -- Right number
type OptTy=Maybe Type -- optional type annoation
data Stmt={-Inline [ArrExp] [ArrExp]
   | Sgl ArrExp
   |-}
     Where ArrExp String
   | EBlk
   | Cpd ArrExp OptTy (Maybe Char) [(Op,RHSArrExp)]
   | MacroProto [String] {-String [String]-}
   | MacroUse [(String,Bool)] String [String] -- name out parameters name inparameters
   deriving (Eq,Show)

pCons::a->Parser a
pCons f cs=Just(f,cs)

--fl: "fill" slot in LHS structure, scrapping parse if nothing parsed
--flB: fill bool slot, defaulting to False if no parse,
--flM: fill Maybe slot, defaulting to Nothing if no parse
--flL: fill List slot, defaulting to [] if no parse
fl::Parser (a->b)->Parser a->Parser b
fl p1 p2 cs=case p1 cs of
  Nothing->Nothing
  Just(f,cs')->case p2 cs' of
       Nothing->Nothing
       Just(v,cs'')->Just(f v,cs'')

fSyntax::Parser a->Parser b->Parser a
fSyntax a b c=case a c of
  Nothing->Nothing
  r@(Just(v,c'))->case b c' of
    Nothing->r
    Just(_,c'')->Just(v,c'')

flB::Parser (Bool->b)->Parser a->Parser b
flB p1 p2 cs=case p1 (dropWS' cs) of
  Nothing->Nothing
  Just(f,cs')->case p2 (dropWS' cs') of
       Nothing->Just(f False,cs')
       Just(v,cs'')->Just(f True,cs'')

flM::Parser (Maybe a->b)->Parser a->Parser b
flM p1 p2 cs=case p1 (dropWS' cs) of
  Nothing->Nothing
  Just(f,cs')->case p2 (dropWS' cs') of
       Nothing->Just(f Nothing,cs')
       Just(v,cs'')->Just(f (Just v),cs'')

flL::Parser ([a]->b)->Parser a->Parser b
flL p1 p2 cs=case p1 (dropWS' cs) of
  Nothing->Nothing
  Just(f,cs')->let (Just(v,cs''))=pPEList pNull p2 cs' in Just(f v,cs'')

flSL::Parser c->Parser ([a]->b)->Parser a->Parser b
flSL pSep p1 p2 cs=case p1 (dropWS' cs) of
  Nothing->Nothing
  Just(f,cs')->let (Just(v,cs''))=pPEList pSep p2 cs' in Just(f v,cs'')

pSpc=pNull
pComma=pChar ','

(@>):: Parser a->Parser b->Parser b
p @> q = (p `pSeq` q) `raise` snd
(<@):: Parser a->Parser b->Parser a
p <@ q = (p `pSeq` q) `raise` fst

pBListArrExps::Bool->Parser [ArrExp]
pBListArrExps lhs=(pChar '[') @> (pNEList pEntry) <@ (pChar ']')
  where pEntry=if lhs then pLHSIdxExp else pIdxExp

pMacroUse=(pCons MacroUse) `cSL` pLHS <@ pMatchStr "=" `fl` pString `cSL` pString
  where pLHS=(pString `raise` (,)) `flB` ((pChar conceptual_marker) `raise` (\_->True))
        cSL=flSL pComma

pDefinition = (pMatchStr "define") @> (pCons MacroProto) `cSL` pString -- <@ pMatchStr "=" `fl` pString `flL` pString <@ pMatchStr "{"
  where cSL=flSL pComma
tryParserList :: [Parser a]-> Parser a
tryParserList [] _ = Nothing
tryParserList (p:ps) cs=case p cs of
  Nothing ->tryParserList ps cs
  r@(Just(_,_))->r

pAll :: Parser ()
pAll cs=Just((),[])
pNull cs=Just((),cs)

pLineParser::[Parser a]-> Parser a
pLineParser ps cs=(tryParserList ps) (dropWS' cs) --((tryParserList ps) `flB` pCommentToEOL) (dropWS' cs)
-- a matching "to end-of-line" comment parser just returns the incoming parse-value
pCommentToEOL :: Parser ()
pCommentToEOL = (dropWS (pMatchStr "##" <@ pAll))

--to be safe we require everything on the line to have been understood to say that we've parsed the line
allAccounted::Parser a->String->Maybe a
allAccounted p cs=case p cs of
  Nothing->Nothing
  Just(v,r)->if all isSpace r then Just v else Nothing

pLine=pLineParser [pEBlk,pWhere,{-pDefinition,-}pMacroUse,pStatement]

pCharStr prd=(pPredChar' prd) `raise` (\x->[x])
--pWhere::Parser Stmt
pWhere=(pMatchStr "where") @> (pCons Where) `fl` pIdxExp <@ (pMatchStr "=>") `fl` (pCharStr isGenAlpha) <@ (pMatchStr "{")

pEBlk = (pCons EBlk) <@ (pChar '}')

pMatchStr::String->Parser ()
pMatchStr xs cs={-dropWS-} (if xs `isPrefixOf` cs then Just ((),drop (length xs) cs) else Nothing)
isPrefixOf xs ys= xs==(take (length xs) ys)

fmt::Stmt->String
--fmt (Sgl a)="Single "++show a++";"
fmt (Cpd l a t as)="Cmpd"++show l++" "++show a++" "++show t++" "++show as++";"

type Parser a=String->Maybe(a,String)

numberLines s=zip [1..] (lines s)
--parseAssgn::Int->String->(Bool,String)

dropWS::Parser a->Parser a
dropWS p cs=p (dropWhile isSpace cs)
dropWS' cs=dropWhile isSpace cs

pPredChar::(Char->Bool)->(Char->a)->Parser a
pPredChar pred upd []=Nothing
pPredChar pred f cs@(c:cs')
 |pred c=Just(f c,cs')
 |otherwise=Nothing

pPredChar'::(Char->Bool)->Parser Char
pPredChar' pred []=Nothing
pPredChar' pred cs@(c:cs')
 |pred c=Just(c,cs')
 |otherwise=Nothing


--parse a string into an ArrExp identifier (everything starts off virtual)
pIdent::Parser ArrExp
pIdent=raise pNString (\cs->Arr cs [] False)

--parse an number or identifier like string required to start with alphabetical character
pNString::Parser String
pNString=pNEList (pPredChar' isGenAlphaNum)

--parse an "identifier-like" string required to start with alphabetical character
pString,pGenNumber::Parser String
pString=(pPredChar isGenAlpha (\x y->(x:y))) `flL` (pPredChar' isGenAlphaNum)
pGenNumber=(pPredChar (\c->c>='0' && c<='9') (\x y->(x:y))) `flL` (pPredChar' isGenAlphaNum)

pChar::Char->Parser Char
pChar f (c:cs) | f==c=Just(c,cs)
pChar _ _ = Nothing

pPredCharUpd::(Char->Bool)->(Char->a->a)->Parser (a->a)
pPredCharUpd pred upd []=Nothing
pPredCharUpd pred upd cs@(c:cs')
 |pred c   =Just(upd c,cs')
 |otherwise=Nothing
{-
pPEList::(Parser l,Parser s,Parser r)->Parser a->Parser [a]
pPEList (l,s,r) p cs=l @>
  where
   pPEList' c0=case (pSep @> p) c0 of
     Nothing->Just([],c0)
     Just(v,c1)->let (Just(v',c2))=pEList' c2 in Just(v:v',c2)
-}
--parse possibly empty list
pPEList::Parser b->Parser a->Parser [a]
pPEList pSep p cs=Just(case p cs of
  Nothing->([],cs)
  Just(v,cs')->let (Just(vs,cs''))=pPEList pSep p cs' in (v:vs,cs'')
  )
--parse list which must have at least one entry
pNEList::Parser a->Parser [a]
pNEList p cs=case p cs of
  Nothing->Nothing
  Just(v,cs')->let (Just(vs,cs''))=pPEList pNull p cs' in Just(v:vs,cs'')

--slotIn::Parser a->(a->b->b)->Parser b
pIdxExpBase::Parser (Bool->ArrExp)
pIdxExpBase=(pCons Arr) `fl` pString `fSyntax` (pChar array_index_sep) `fl` pIdxs
  where pIdxs=(pPEList pNull (pCharStr isGenAlphaNum))

pIdxExp::Parser ArrExp
pIdxExp=pIdxExpBase `fl` (pCons False)

array_index_sep='.'
conceptual_marker='!'
pLHSIdxExp::Parser ArrExp
pLHSIdxExp=pIdxExpBase `flB` (pChar conceptual_marker)

pPostMod::Parser a->Parser (a->a)->Parser a
pPostMod p q cs=case p cs of
 Nothing->Nothing
 r@(Just (v,cs'))->case q cs' of
                   Nothing->r
                   Just(f,cs'')->Just (f v,cs'')

pAsgnOp::Parser (Maybe Char)
pAsgnOp=(pChar '=') @> (pCons id) `flM` (pPredChar' (`elem` "+*&|"))

--should sequencing allow discardable whitespace between tokens
pSeqIn::Bool->Parser a->Parser b->Parser (a,b)
pSeqIn allowWS p q cs=case p cs of
 Nothing->Nothing
 r@(Just(v,cs'))->case q (if allowWS then dropWS' cs' else cs') of
            Nothing->Nothing
            Just(w,cs'')->Just((v,w),cs'')

pSeq::Parser a->Parser b->Parser (a,b)
pSeq=pSeqIn True
pSeqNW=pSeqIn False

--add "converter" to output of a parser to new kind of output
raise::Parser a->(a->b)->Parser b
raise p f cs=case p cs of
  Nothing->Nothing
  Just(v,cs')->Just(f v,cs')

pOptionalBracketed::Char->Char->Parser a->Parser a
pOptionalBracketed l r p []=Nothing
pOptionalBracketed l r p cs@(c:cs')=let bracketed=(l==c) in
  case p (if bracketed then cs' else cs) of
        Nothing->Nothing
        Just(v,cs'')->if bracketed then if null cs'' || r/=head cs'' then Nothing else Just(v,tail cs'')
                      else Just(v,cs'')

pORB=pOptionalBracketed '(' ')'
pOSB=pOptionalBracketed '[' ']'
-- type annotation is eg, ':Type128'
pTypeAnnote::Parser Type
pTypeAnnote = (dropWS ((pPredChar' (==':')) @> (dropWS pString)))

pTyBinOp::Parser Op
pTyBinOp = dropWS(pORB ((pCons Op) `fl` ((pPredChar' (`elem` "+-*/&|^2<>")) `raise` (\x->[x])) `flM` pTypeAnnote))

pEAlt::Parser a->Parser b->Parser (Either a b)
pEAlt a b c=case a c of
   Nothing->case b c of
      Nothing->Nothing
      Just(w,c'')->Just(Right w,c'')
   Just(v,c')->Just (Left v,c')

pRHSArrExp = pEAlt pIdxExp pGenNumber

--one odd syntax <-> AST mismatch: write LHS types on LHS identifier but store on the assignment operator
--also need to stick a dummy operator on front of list
pStatement::Parser Stmt
pStatement=(pCons Cpd) `fl` pLHSIdxExp `flM` pTypeAnnote  `fl` pAsgnOp `fl` (pCombOps)
  where
   pCombOp::Parser (Op,RHSArrExp)
   pCombOp=pTyBinOp `pSeq` pRHSArrExp
   pCombOps=(pRHSArrExp `raise` (\x->((Op "X" Nothing,x):))) `flL` pCombOp

processStrB::String->String
processStrB=unlines.map (show.fromJust.(allAccounted pLine)).lines

processStrF::String->String
processStrF=unlines.map (fmt.fromJust.(allAccounted pLine)).lines

pFile s=do j<-readFile s
           (putStr.processStrF) j

processStrE::String->String
processStrE=unlines.map (show.pLine).(filter (not.all isSpace) .lines)

pFileE s=do j<-readFile s
            (putStr.processStrE) j

pp n=pFileE ("TESTS/parseT"++show n++".txt")
