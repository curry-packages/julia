------------------------------------------------------------------------------
--- This module contains an example to generate the following Julia script
--- which computes and prints the 10th factorial numbers:
---
---     function fac(n::Int64) :: Int64
---       p = 1
---       for i in 1:n
---         p = p*i
---       end
---       return p
---     end
---     
---     println(fac(10))
---
------------------------------------------------------------------------------

import Text.Pretty  ( pPrint )

import Language.Julia.Pretty ( ppScript )
import Language.Julia.Types

--- Generate the factorial function.
jlFac :: JLTop
jlFac =
  JLFDecl "fac" [(0, Just JLInt64)] (Just JLInt64)
    [ JLAssign (JLIVar 1) (JLInt 1)
    , JLFor 2 (JLInt 1) (JLIVar 0)
            [ JLAssign (JLIVar 1) (JLOp "*" (JLIVar 1) (JLIVar 2)) ]
    , JLReturn (JLIVar 1)
    ]

--- Generate the script with the factorial function and the main statement.
jlFacScript :: [JLTop]
jlFacScript = [ jlFac, JLStat (JLPCall "println" [JLFCall "fac" [JLInt 10]])]

--- Pretty print the generated script.
--- If one has `julia` in the path, one could also write the script
--- into a file or provide it as input to the `julia` command.
main :: IO ()
main = putStrLn $ pPrint $ ppScript jlFacScript
