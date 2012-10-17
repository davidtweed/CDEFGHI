

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
-- Asgn' Basic Assignment
data Asgn=Asgn Bool | Reduce Bool Char deriving (Eq,Show)
-- Asgn assignment with type spec on LHS
--type Asgn=(Asgn',OptTy) -- assignment with optional LHS type
data Op=Op String OptTy deriving (Eq,Show) -- operator with type it occurs in
type OptTy=Maybe Type -- optional type annoation
data MacroInfo = MI [ArrExp] String [ArrExp] deriving (Eq,Show)
data Stmt=Inline [ArrExp] [ArrExp]
   | Sgl ArrExp
   | Where ArrExp String
   | EBlk
   | Cpd {-ArrExp-} Asgn OptTy [ArrExp] [Op]
   | MacroProto MacroInfo
   | MacroUse MacroInfo -- name out parameters name inparameters
   deriving (Eq,Show)

data Def=Def MacroInfo [Stmt]

pCons::a->Parser a
pCons f cs=Just(f,cs)

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

pMI=((pBListArrExps True) `pSeq` pMatchStr "=" `pSeq` pString `pSeq` (pBListArrExps False))
           `raise` (\(((a,b),c),d)->MI a c d)

--pMI=(pCons MI) `flL` pLHSIdxExp `fl` pString `flL` pIdxExp

pMacroUse=pMI `raise` (\x->MacroUse x)

pDefinition = ((pMatchStr "define") `pSeq` pMI `pSeq` pMatchStr "{") `raise` (\((_,y),_)->MacroProto y)

tryParserList :: [Parser a]-> Parser a
tryParserList [] _ = Nothing
tryParserList (p:ps) cs=case p cs of
  Nothing ->tryParserList ps cs
  r@(Just(_,_))->r

pAnything :: Parser ()
pAnything cs=Just((),[])

pLineParser::[Parser a]-> Parser a
pLineParser ps cs=((tryParserList ps) `pOptPostMod` pCommentToEOL) (dropWS' cs)
-- a matching "to end-of-line" comment parser just returns the incoming parse-value
pCommentToEOL :: Parser (a->a)
pCommentToEOL = (dropWS (pMatchStr "##" `pSeq` pAnything)) `raise` (\_ v->v)

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
fmt (Cpd a t as os)="Cmpd"++show a++" "++show t++" "++show as++show os++";"

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
pString::Parser String
pString=pNEList (pChar isGenAlphaNum)

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
pIdxExpBase=(pCons Arr) `fl` pString `flL` ((pPredChar' (array_index_sep==)) @> pIdxs)
  where pIdxs=(pPEList (pChar isGenAlphaNum))

pIdxExp::Parser ArrExp
pIdxExp=pIdxExpBase `raise` (\f->f False)

array_index_sep='.'
conceptual_maker='!'
pLHSIdxExp::Parser ArrExp
pLHSIdxExp=pIdxExpBase `flB` (pChar (conceptual_maker==))

pPostMod::Parser a->Parser (a->a)->Parser a
pPostMod p q cs=case p cs of
 Nothing->Nothing
 r@(Just (v,cs'))->case q cs' of
                   Nothing->r
                   Just(f,cs'')->Just (f v,cs'')

pAsgnOp::Parser Asgn
pAsgnOp=(pPredChar ('='==) (\_->Asgn True))
        `pPostMod`
        (pPredCharUpd (`elem` "+*&|") (\c (Asgn i)->Reduce i c))

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
   pSimple=(pLHSIdxExp `pSeq` pOptTy `pSeq` pAsgnOp `pSeq` pIdxExp) `raise` (\ (((e1,t),a),e2)->Cpd a t [e2,e1] [])
   pCombOp::Parser (Op,ArrExp)
   pCombOp=pTyBinOp `pSeq` pIdxExp
   pBinOp=pCombOp `raise` (\ (op,idExpr) (Cpd a t es os)->Cpd a t (idExpr:es) (op:os))
   canonicalise x@(Sgl _)=x
   canonicalise (Cpd a t es os)=Cpd a t (reverse es) (reverse os)

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
