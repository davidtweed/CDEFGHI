

import Control.Monad (liftM)
import Data.Char
import Language.Haskell.Extension (Extension)
import Maybe

-- -----------------------------------------------------------------------------

isGenAlpha c=(c>='a' && c<='z') || (c>='A' && c<='Z') || c=='_' || c=='%'
isGenAlphaNum c=isGenAlpha c || (c>='0' && c<='9')

type Type=String
-- Arr Entity IndexesString
data ArrExp=Arr String String deriving (Eq,Show)
-- Asgn' Basic Assignment
data Asgn=Asgn Bool | Reduce Bool Char deriving (Eq,Show)
-- Asgn assignment with type spec on LHS
--type Asgn=(Asgn',OptTy) -- assignment with optional LHS type
data Op=Op String OptTy deriving (Eq,Show) -- operator with type it occurs in
type OptTy=Maybe Type -- optional type annoation
data MacroInfo = MI [ArrExp] String [ArrExp] deriving (Eq,Show)
data Stmt=Inline [ArrExp] [ArrExp]
   | Sgl ArrExp
   | Where String ArrExp
   | EBlk
   | Cpd Asgn OptTy [ArrExp] [Op]
   | MacroProto MacroInfo
   | MacroUse MacroInfo -- name out parameters name inparameters
   deriving (Eq,Show)

data Def=Def MacroInfo [Stmt]

pBListArrExps=((pPredChar (=='[') (\_->[])) `pZOrMoreMod` (pIdxExp `raise` (:)) `pSeq`
    (pMatchStr "]")) `raise` (reverse.fst)

pMI=(pBListArrExps `pSeq` pMatchStr "=" `pSeq` pString `pSeq` pBListArrExps)
           `raise` (\(((a,b),c),d)->MI a c d)

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

--pWhere::Parser Stmt
pWhere = ((pMatchStr "where") `pSeq` pIdxExp `pSeq` (pMatchStr "=>") `pSeq`  (pPredChar' isGenAlpha)
          `pSeq` (pMatchStr "{")) `raise` (\((((a,b),c),d),e)->Where [d] b)
pEBlk = dropWS (pPredChar (=='}') (\_->EBlk))
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


--parse an "identifier-like" string required to start with alphabetical character
pString::Parser String
pString=dropWS
      ((((pPredChar isGenAlpha (\c->[c])) `pZOrMoreMod` (pPredCharUpd isGenAlphaNum (:))))
      `raise` reverse)

--parse a string into an ArrExp identifier
pIdent::Parser ArrExp
pIdent=raise pString (\cs->Arr cs "")

pPredCharUpd::(Char->Bool)->(Char->a->a)->Parser (a->a)
pPredCharUpd pred upd []=Nothing
pPredCharUpd pred upd cs@(c:cs')
 |pred c   =Just(upd c,cs')
 |otherwise=Nothing

pIdxExp::Parser ArrExp
pIdxExp=((pIdent
     `pZOrMoreMod`
     ((pPredChar' (=='.'))`raise` (\c v->v)))
     `pZOrMoreMod` pIdxs)
     `raise` (\(Arr a b)->Arr a (reverse b))
  where pIdxs::Parser (ArrExp->ArrExp)
        pIdxs=(pPredCharUpd isGenAlpha (\c (Arr n i)->Arr n (c:i)))


pPostMod::Parser a->Parser (a->a)->Parser a
pPostMod p q cs=case p cs of
 Nothing->Nothing
 r@(Just (v,cs'))->case q cs' of
                   Nothing->r
                   Just(f,cs'')->Just (f v,cs'')

pAsgnOp::Parser Asgn
pAsgnOp=((pPredChar ('='==) (\_->Asgn True))
        `pPostMod`
        ((pPredCharUpd ('!'==) (\_ _->Asgn False))))
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

-- type annotation is eg, ':Type128'
pTypeAnnote::Parser Type
pTypeAnnote = (dropWS ((pPredChar' (==':')) `pSeq` (dropWS pString))) `raise` snd

pTyBinOp::Parser Op
pTyBinOp = pBinOpSyms `pPostMod` (pTypeAnnote `raise` (\ty (Op op _)->Op op (Just ty)))
  where pBinOpSyms=(pPredChar' (`elem` "+*/&|")) `raise` (\c->Op [c] Nothing)

--one odd syntax <-> AST mismatch: write LHS types on LHS identifier but store on the assignment operator

pStatement::Parser Stmt
pStatement=(pSimple `pZOrMoreMod` (dropWS pBinOp)) `raise` canonicalise
  where
   pOptTy::Parser OptTy
   pOptTy c=case pTypeAnnote c of
       Nothing->Just(Nothing,c)
       (Just(t,c'))->Just(Just t,c')
   pSimple::Parser Stmt
   pSimple=(pIdxExp `pSeq` pOptTy `pSeq` pAsgnOp `pSeq` pIdxExp) `raise` (\ (((e1,t),a),e2)->Cpd a t [e2,e1] [])
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
