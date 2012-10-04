

import Control.Monad (liftM)
import Data.Char
import Language.Haskell.Extension (Extension)
import Maybe

-- -----------------------------------------------------------------------------

isGenAlpha c=(c>='a' && c<='z') || (c>='A' && c<='Z') || c=='_'
type Type=String
data ArrExp=Arr String String deriving (Eq,Show)
data Asgn'=Asgn Bool | Reduce Bool Char deriving (Eq,Show)
type Asgn=(Asgn',OptTy) -- assignment with optional LHS type
data Op=Op String OptTy deriving (Eq,Show) -- operator with type it occurs in
type OptTy=Maybe Type -- optional type annoation
data Stmt=Sgl ArrExp | Cpd Asgn [ArrExp] [Op]  deriving (Eq,Show)


fmt::Stmt->String
fmt (Sgl a)="Single "++show a++";"
fmt (Cpd a as os)="Cmpd"++show a++show as++show os++";"

type Parser a=String->Maybe(a,String)

numberLines s=zip [1..] (lines s)
--parseAssgn::Int->String->(Bool,String)

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
    where --f::a->Parser a
          f v s=case q s of
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

pString::Parser String
pString=dropWS
       ((pPredChar isGenAlpha (\c->[c]))
       `pZOrMoreMod`
       (pPredCharUpd isGenAlpha (:)))

pIdent::Parser ArrExp
pIdent=raise pString (\cs->Arr cs "")

pPredCharUpd::(Char->Bool)->(Char->a->a)->Parser (a->a)
pPredCharUpd pred upd []=Nothing
pPredCharUpd pred upd cs@(c:cs')
 |pred c   =Just(upd c,cs')
 |otherwise=Nothing

pIdxExp::Parser ArrExp
pIdxExp=(pIdent
     `pZOrMoreMod`
     ((pPredChar' (=='.'))`raise` (\c v->v)))
     `pZOrMoreMod`
     (pPredCharUpd isGenAlpha (\c (Arr n i)->Arr n (c:i)))

pPostMod::Parser a->Parser (a->a)->Parser a
pPostMod p q cs=case p cs of
 Nothing->Nothing
 r@(Just (v,cs'))->case q cs' of
                   Nothing->r
                   Just(f,cs'')->Just (f v,cs'')

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

raise::Parser a->(a->b)->Parser b
raise p f cs=case p cs of
  Nothing->Nothing
  Just(v,cs')->Just(f v,cs')

-- type annotation is eg, ': Type128'
pTypeAnnote::Parser Type
pTypeAnnote = raise ((pPredChar' (==':')) `pSeq` (pString)) snd



pStatement::Parser Stmt
pStatement=pPostMod pSimple (dropWS pBinOp)
  where
   pSimple=(pIdxExp `pSeq` pAsgnOp `pSeq` pIdxExp)
          `raise` (\ ((e1,a),e2)->Cpd a [e2,e1] [])
   pCombOp=(pPredChar' (`elem` "+*&|")) `pSeq` pIdxExp
   pBinOp=pCombOp `raise` (\ (c,e) (Cpd a es os)->Cpd a (e:es) ([c]:os))

processStrB::String->String
processStrB=unlines.map (show.fst.fromJust.pStatement).lines

processStrF::String->String
processStrF=unlines.map (fmt.fst.fromJust.pStatement).lines

pFile s=do j<-readFile s
           (putStr.processStrF) j
