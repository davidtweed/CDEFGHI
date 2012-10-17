

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
type OptTy=Maybe Type -- optional type annoation
data Stmt=Inline [ArrExp] [ArrExp]
   | Sgl ArrExp
   | Where ArrExp String
   | EBlk
   | Cpd ArrExp OptTy (Maybe Char) [ArrExp] [Op]
   | MacroProto [String] String [String]
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

flB::Parser (Bool->b)->Parser a->Parser b
flB p1 p2 cs=case p1 cs of
  Nothing->Nothing
  Just(f,cs')->case p2 cs' of
       Nothing->Just(f False,cs')
       Just(v,cs'')->Just(f True,cs'')

flM::Parser (Maybe a->b)->Parser a->Parser b
flM p1 p2 cs=case p1 cs of
  Nothing->Nothing
  Just(f,cs')->case p2 cs' of
       Nothing->Just(f Nothing,cs')
       Just(v,cs'')->Just(f (Just v),cs'')

flL::Parser ([a]->b)->Parser a->Parser b
flL p1 p2 cs=case p1 cs of
  Nothing->Nothing
  Just(f,cs')->let (Just(v,cs''))=pPEList p2 cs' in Just(f v,cs'')

(@>):: Parser a->Parser b->Parser b
p @> q = (p `pSeq` q) `raise` snd
(<@):: Parser a->Parser b->Parser a
p <@ q = (p `pSeq` q) `raise` fst

pBListArrExps::Bool->Parser [ArrExp]
pBListArrExps lhs=(pChar (=='[')) @> (pNEList pEntry) <@ (pChar (==']'))
  where pEntry=if lhs then pLHSIdxExp else pIdxExp

pMacroUse=(pCons MacroUse) `flL` pLHS <@ pMatchStr "=" `fl` pString `flL` pString
  where pLHS=(pString `raise` (,)) `flB` (pPredChar (conceptual_marker==) (\_->True))

pDefinition = (pMatchStr "define") @> (pCons MacroProto) `flL` pString <@ pMatchStr "=" `fl` pString `flL` pString <@ pMatchStr "{"

tryParserList :: [Parser a]-> Parser a
tryParserList [] _ = Nothing
tryParserList (p:ps) cs=case p cs of
  Nothing ->tryParserList ps cs
  r@(Just(_,_))->r

pAnything :: Parser ()
pAnything cs=Just((),[])

pLineParser::[Parser a]-> Parser a
pLineParser ps cs=(tryParserList ps) (dropWS' cs) --((tryParserList ps) `flB` pCommentToEOL) (dropWS' cs)
-- a matching "to end-of-line" comment parser just returns the incoming parse-value
pCommentToEOL :: Parser ()
pCommentToEOL = (dropWS (pMatchStr "##" <@ pAnything))

--to be safe we require everything on the line to have been understood to say that we've parsed the line
allAccounted::Parser a->String->Maybe a
allAccounted p cs=case p cs of
  Nothing->Nothing
  Just(v,r)->if all isSpace r then Just v else Nothing

pLine=pLineParser [pEBlk,pWhere,pDefinition,pMacroUse,pStatement]

pCharStr prd=(pChar prd) `raise` (\x->[x])
--pWhere::Parser Stmt
pWhere=(pMatchStr "where") @> (pCons Where) `fl` pIdxExp <@ (pMatchStr "=>") `fl` (pCharStr isGenAlpha) <@ (pMatchStr "{")

pEBlk = (pCons EBlk) <@ (pChar (=='}'))

pMatchStr::String->Parser ()
pMatchStr xs cs={-dropWS-} (if xs `isPrefixOf` cs then Just ((),drop (length xs) cs) else Nothing)
isPrefixOf xs ys= xs==(take (length xs) ys)

fmt::Stmt->String
fmt (Sgl a)="Single "++show a++";"
fmt (Cpd l a t as os)="Cmpd"++show l++" "++show a++" "++show t++" "++show as++show os++";"

type Parser a=String->Maybe(a,String)

numberLines s=zip [1..] (lines s)
--parseAssgn::Int->String->(Bool,String)

dropWS::Parser a->Parser a
dropWS p cs=p (dropWhile isSpace cs)
dropWS' cs=dropWhile isSpace cs

pOptPostMod::Parser a->Parser (a->a)->Parser a
pOptPostMod p1 p2 s=case p1 s of
 Nothing->Nothing
 r@(Just (v,s'))->case p2 s' of
              Nothing->r
              Just (f,s'')->Just(f v,s'')

pZOrMoreMod::Parser a->Parser (a->a)->Parser a
pZOrMoreMod p q cs=case p cs of
 Nothing->Nothing
 Just (v,cs')->f v cs'
    where f v s=case q s of
                Nothing   ->Just (v,s)
                Just (g,t)->f (g v) t

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
pIdent=raise pString (\cs->Arr cs [] False)

--parse an "identifier-like" string required to start with alphabetical character
pNString::Parser String
pNString=pNEList (pChar isGenAlphaNum)

pString::Parser String
pString=(pPredChar isGenAlpha (\x y->(x:y))) `flL` (pChar isGenAlphaNum)

pChar::(Char->Bool)->Parser Char
pChar f (c:cs) | f c=Just(c,cs)
pChar _ _ = Nothing

pPredCharUpd::(Char->Bool)->(Char->a->a)->Parser (a->a)
pPredCharUpd pred upd []=Nothing
pPredCharUpd pred upd cs@(c:cs')
 |pred c   =Just(upd c,cs')
 |otherwise=Nothing

--parse possibly empty list
pPEList::Parser a->Parser [a]
pPEList p cs=Just(case p cs of
  Nothing->([],cs)
  Just(v,cs')->let (Just(vs,cs''))=pPEList p cs' in (v:vs,cs'')
  )
--parse list which must have at least one entry
pNEList::Parser a->Parser [a]
pNEList p cs=case p cs of
  Nothing->Nothing
  Just(v,cs')->let (Just(vs,cs''))=pPEList p cs' in Just(v:vs,cs'')

--slotIn::Parser a->(a->b->b)->Parser b
pIdxExpBase::Parser (Bool->ArrExp)
pIdxExpBase=(pCons Arr) `fl` pString `fl` ((pPredChar' (array_index_sep==)) @> pIdxs)
  where pIdxs=(pPEList (pCharStr isGenAlphaNum))

pIdxExp::Parser ArrExp
pIdxExp=pIdxExpBase `fl` (pCons False)

array_index_sep='.'
conceptual_marker='!'
pLHSIdxExp::Parser ArrExp
pLHSIdxExp=pIdxExpBase `flB` (pChar (conceptual_marker==))

pPostMod::Parser a->Parser (a->a)->Parser a
pPostMod p q cs=case p cs of
 Nothing->Nothing
 r@(Just (v,cs'))->case q cs' of
                   Nothing->r
                   Just(f,cs'')->Just (f v,cs'')

pAsgnOp::Parser (Maybe Char)
pAsgnOp=(pChar ('='==)) @> (pCons id) `flM` (pChar (`elem` "+*&|"))

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
{-
pOptionalBracketed::Char->Char->Parser a->Parser a
pOptionalBracketed l r p cs@(c:cs')=let bracketed=(l==c) in
  case p (if bracketed then cs' else cs) of
    Nothing->Nothing
    Just(v,cs'')->if bracketed && not(r==head cs'') then
-}
-- type annotation is eg, ':Type128'
pTypeAnnote::Parser Type
pTypeAnnote = (dropWS ((pPredChar' (==':')) @> (dropWS pString)))

pTyBinOp::Parser Op
pTyBinOp = (pCons Op) `fl` ((pChar (`elem` "+*/&|")) `raise` (\x->[x])) `flM` pTypeAnnote

--one odd syntax <-> AST mismatch: write LHS types on LHS identifier but store on the assignment operator

pStatement::Parser Stmt
pStatement=(pSimple `pZOrMoreMod` (dropWS pBinOp)) `raise` canonicalise
  where
   pOptTy::Parser OptTy
   pOptTy c=case pTypeAnnote c of
       Nothing->Just(Nothing,c)
       (Just(t,c'))->Just(Just t,c')
   pSimple::Parser Stmt
   pSimple=(pLHSIdxExp `pSeq` pOptTy `pSeq` pAsgnOp `pSeq` pIdxExp) `raise` (\ (((e1,t),a),e2)->Cpd e1 t a [e2] [])
   pCombOp::Parser (Op,ArrExp)
   pCombOp=pTyBinOp `pSeq` pIdxExp
   pBinOp=pCombOp `raise` (\ (op,idExpr) (Cpd l a t es os)->Cpd l a t (idExpr:es) (op:os))
   canonicalise x@(Sgl _)=x
   canonicalise (Cpd l a t es os)=Cpd l a t (reverse es) (reverse os)
{-

pStatement::Parser Stmt
pStatement=(pCons Cpd) `fl` pLHSIdxExp `flM` pTypeAnnote  `fl` pAsgnOp `flL` pIdxExp
-}
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

pp=pFileE "TESTS/parseT1.txt"
