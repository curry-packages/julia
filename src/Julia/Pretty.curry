------------------------------------------------------------------------------
--- This module contains a pretty printer for Julia programs.
---
--- @author Michael Hanus
--- @version June 2020
------------------------------------------------------------------------------

module Julia.Pretty where

import Text.Pretty

import Julia.Types

--- Pretty print a Julia type.
ppType :: JLType -> Doc
ppType JLBoolType   = text "Bool"
ppType JLInt32      = text "Int32"
ppType JLInt64      = text "Int64"
ppType JLFloat64    = text "Float64"
ppType JLStringType = text "String"
ppType (JLStruct n) = text n
ppType (JLArray ts) = text "Array" <>
                      braces (hcat (punctuate comma (map ppType ts)))

--- Pretty print an optional type annotation.
ppTypeAnn :: Maybe JLType -> Doc
ppTypeAnn Nothing   = empty
ppTypeAnn (Just jt) = text " ::" <+> ppType jt

--- Pretty print a list of comma separated expressions.
ppExps :: [JLExp] -> Doc
ppExps = hsep . punctuate comma . map ppExp

--- Pretty print a Julia expression.
ppExp :: JLExp -> Doc
ppExp (JLInt n)         = int n
ppExp (JLFloat f)       = float f
ppExp (JLBool b)        = text (if b then "true" else "false")
ppExp (JLString s)      = ppStringLit s
ppExp (JLIVar v)        = ppVar v
ppExp (JLSVar s)        = text s
ppExp (JLArrayAcc a i)  = ppExp a <> brackets (ppExp i)
ppExp (JLArrayInit es)  = brackets (ppExps es)
ppExp (JLStructAcc e c) = ppExp e <> char '.' <> text c
ppExp (JLOp op e1 e2)   = parens (ppExp e1 <+> text op <+> ppExp e2)
ppExp (JLFCall f es)    = text f <> parens (ppExps es)

-- Pretty print a Julia string literal. Encode backslashes
ppStringLit :: String -> Doc
ppStringLit s = char '"' <> text (concatMap encChar s) <> char '"'
 where
  encChar c | c == '$'  = ['\\','$']
            | otherwise = [c]

--- Pretty print a variable. Variables are printed as x0, x1, ...
ppVar :: Int -> Doc
ppVar v = text . ('x':) . show $ v

--- Pretty print Julia statements.
ppStms :: [JLStm] -> Doc
ppStms = vcat . map ppStm

--- Pretty print a Julia statement.
ppStm :: JLStm -> Doc
ppStm (JLAssign e1 e2) = ppExp e1 <+> char '=' <+> ppExp e2
ppStm (JLPCall f es)   = text f <> parens (ppExps es)
ppStm (JLReturn e)     = text "return" <+> ppExp e
ppStm (JLWhile b body) =
  nest 2 (text "while" <+> ppExp b $$ ppStms body) $$ text "end"
ppStm (JLFor v lb ub body) =
  nest 2 (text "for" <+> ppVar v <+> text "in" <+>
          ppExp lb <> char ':' <> ppExp ub $$ ppStms body) $$ text "end"
ppStm (JLIf b ts fs)   = nest 2 (text "if" <+> ppExp b $$ ppStms ts) $$
                         ppElse fs
 where
  ppElse estm = case estm of
    [JLIf eb ets efs] -> nest 2 (text "elseif" <+> ppExp eb $$ ppStms ets) $$
                         ppElse efs
    _                 -> nest 2 (text "else" $$ ppStms estm) $$ text "end"

-- Pretty print a Julia function declaration.
ppTop :: JLTop -> Doc
ppTop (JLStat stm) = ppStm stm
ppTop (JLFDecl f args rtype body) =
 nest 2 (text "function" <+> text f <> parens (ppArgs args) <> ppTypeAnn rtype
           $$ ppStms body)
  $$ text "end"
 where
  ppArgs = hsep . punctuate comma . map ppArg
  ppArg (v,t) = ppVar v <> ppTypeAnn t

-- Pretty print a Julia script.
ppScript :: [JLTop] -> Doc
ppScript = vsepBlank . map ppTop

-- Pretty print a Julia module.
ppModule :: JLModule -> Doc
ppModule (JLModule mname exps imps tops) =
  text "module" <+> text mname <$+$>
  (if null exps
     then empty
     else text "export" <+> hsep (punctuate comma (map text exps))) <$+$>
  vsep (map (\i -> text "using" <+> text i) imps) <$+$>
  ppScript tops <$+$>
  text "end"

------------------------------------------------------------------------------
