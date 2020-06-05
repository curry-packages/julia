------------------------------------------------------------------------------
--- A library to represent a subset of Julia programs.
---
--- @author Michael Hanus
--- @version June 2020
------------------------------------------------------------------------------

module Julia.Types
 where

------------------------------------------------------------------------------
--- Julia types.
data JLType = JLBoolType | JLInt32 | JLInt64 | JLFloat64 | JLStringType
            | JLStruct String
            | JLArray [JLType]

--- Julia expressions.
data JLExp = JLBool      Bool
           | JLInt       Int
           | JLFloat     Float
           | JLString    String
           | JLIVar      Int                -- indexed variable
           | JLSVar      String             -- other variable expression
           | JLArrayAcc  JLExp JLExp        -- array access to element
           | JLArrayInit [JLExp]            -- array initializer
           | JLStructAcc JLExp String       -- structure access to component
           | JLOp        String JLExp JLExp -- binary operator
           | JLFCall     String [JLExp]     -- function call

--- Julia statements.
data JLStm = JLAssign  JLExp JLExp
           | JLIf      JLExp  [JLStm] [JLStm]
           | JLWhile   JLExp  [JLStm]
           | JLFor     Int JLExp JLExp [JLStm] -- variable/lower/upper/body
           | JLPCall   String [JLExp]
           | JLReturn  JLExp

--- Julia top-level entities occurring in scripts.
--- These are function declarations or statements.
--- A function declaration consists of a name, indexed arguments
--- with optional types, an optional result type and the body
--- represented by a list of statements.
data JLTop =
    JLFDecl String [(Int, Maybe JLType)] (Maybe JLType) [JLStm]
  | JLStat  JLStm

--- Julia module consisting of the module name, exported names,
--- imported modules, and a list of top-level entities.
data JLModule = JLModule String [String] [String] [JLTop]

------------------------------------------------------------------------------
