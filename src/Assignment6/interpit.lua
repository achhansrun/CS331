-- interpit.lua  UNFINISHED
-- Glenn G. Chappell
-- 2024-04-02
--
-- For CS 331 Spring 2024
-- Interpret AST from parseit.parse
-- Solution to Assignment 6, Exercise B


-- *** To run a Nilgai program, use nilgai.lua, which uses this file.


-- *********************************************************************
-- Module Table Initialization
-- *********************************************************************


local interpit = {}  -- Our module


-- *********************************************************************
-- Symbolic Constants for AST
-- *********************************************************************


local PROGRAM      = 1
local EMPTY_STMT   = 2
local OUTPUT_STMT  = 3
local RETURN_STMT  = 4
local ASSN_STMT    = 5
local FUNC_CALL    = 6
local FUNC_DEF     = 7
local IF_STMT      = 8
local WHILE_LOOP   = 9
local STRLIT_OUT   = 10
local EOL_OUT      = 11
local CHAR_CALL    = 12
local BIN_OP       = 13
local UN_OP        = 14
local NUMLIT_VAL   = 15
local BOOLLIT_VAL  = 16
local INPUT_CALL   = 17
local RAND_CALL    = 18
local SIMPLE_VAR   = 19
local ARRAY_VAR    = 20


-- *********************************************************************
-- Utility Functions
-- *********************************************************************


-- numToInt
-- Given a number, return the number rounded toward zero.
local function numToInt(n)
    assert(type(n) == "number")

    if n >= 0 then
        return math.floor(n)
    else
        return math.ceil(n)
    end
end


-- strToNum
-- Given a string, attempt to interpret it as an integer. If this
-- succeeds, return the integer. Otherwise, return 0.
local function strToNum(s)
    assert(type(s) == "string")

    -- Try to do string -> number conversion; make protected call
    -- (pcall), so we can handle errors.
    local success, value = pcall(function() return tonumber(s) end)

    -- Return integer value, or 0 on error.
    if success and value ~= nil then
        return numToInt(value)
    else
        return 0
    end
end


-- numToStr
-- Given a number, return its string form.
local function numToStr(n)
    assert(type(n) == "number")

    return tostring(n)
end


-- boolToInt
-- Given a boolean, return 1 if it is true, 0 if it is false.
local function boolToInt(b)
    assert(type(b) == "boolean")

    if b then
        return 1
    else
        return 0
    end
end


-- astToStr
-- Given an AST, produce a string holding the AST in (roughly) Lua form,
-- with numbers replaced by names of symbolic constants used in parseit.
-- A table is assumed to represent an array.
-- See the Assignment 4 description for the AST Specification.
--
-- THIS FUNCTION IS INTENDED FOR USE IN DEBUGGING ONLY!
-- IT SHOULD NOT BE CALLED IN THE FINAL VERSION OF THE CODE.
function astToStr(x)
    local symbolNames = {
        "PROGRAM", "EMPTY_STMT", "OUTPUT_STMT", "RETURN_STMT",
        "ASSN_STMT", "FUNC_CALL", "FUNC_DEF", "IF_STMT", "WHILE_LOOP",
        "STRLIT_OUT", "EOL_OUT", "CHAR_CALL", "BIN_OP", "UN_OP",
        "NUMLIT_VAL", "BOOLLIT_VAL", "INPUT_CALL", "RAND_CALL",
        "SIMPLE_VAR", "ARRAY_VAR",
    }
    if type(x) == "number" then
        local name = symbolNames[x]
        if name == nil then
            return "<Unknown numerical constant: "..x..">"
        else
            return name
        end
    elseif type(x) == "string" then
        return '"'..x..'"'
    elseif type(x) == "boolean" then
        if x then
            return "true"
        else
            return "false"
        end
    elseif type(x) == "table" then
        local first = true
        local result = "{"
        for k = 1, #x do
            if not first then
                result = result .. ","
            end
            result = result .. astToStr(x[k])
            first = false
        end
        result = result .. "}"
        return result
    elseif type(x) == "nil" then
        return "nil"
    else
        return "<"..type(x)..">"
    end
end


-- *********************************************************************
-- Primary Function for Client Code
-- *********************************************************************


-- interp
-- Interpreter, given AST returned by parseit.parse.
-- Parameters:
--   ast    - AST constructed by parseit.parse
--   state  - Table holding Nilgai variables & functions
--            - AST for function xyz is in state.f["xyz"]
--            - Value of simple variable xyz is in state.v["xyz"]
--            - Value of array item xyz[42] is in state.a["xyz"][42]
--   util   - Table with 3 members, all functions:
--            - incall() inputs line, returns string with no newline
--            - outcall(str) outputs str with no added newline
--              To print a newline, do outcall("\n")
--            - random(n), for an integer n, returns a pseudorandom
--              integer from 0 to n-1, or 0 if n < 2.
-- Return Value:
--   state, updated with changed variable values
function interpit.interp(ast, state, util)
    -- Each local interpretation function is given the AST for the
    -- portion of the code it is interpreting. The function-wide
    -- versions of state and until may be used. The function-wide
    -- version of state may be modified as appropriate.


    -- Forward declare local functions
    local interp_program
    local interp_stmt
    local eval_output_arg
    local eval_expr


    -- interp_program
    -- Given the ast for a program, execute it.
    function interp_program(ast)
        -- TODO: WRITE THIS!!!
        assert(type(ast) == "table")
        assert(ast[1] == PROGRAM)
        for i = 2, #ast do
            interp_stmt(ast[i])
        end
    end


    -- interp_stmt
    -- Given the ast for a statement, execute it.
    function interp_stmt(ast)
        -- TODO: WRITE THIS!!!
        assert(type(ast) == "table")
        if ast[1] == EMPTY_STMT then
            -- Do nothing
        elseif ast[1] == OUTPUT_STMT then
            for i = 2, #ast do
                str = eval_output_arg(ast[i])
                util.output(str)
            end
        elseif ast[1] == FUNC_DEF then
            funcname = ast[2]
            funcbody = ast[3]
            state.f[funcname] = funcbody
        elseif ast[1] == FUNC_CALL then
            funcname = ast[2]
            funcbody = state.f[ast[2]]
            if funcbody == nil then
                funcbody = { PROGRAM }
            end
            interp_program(funcbody)
        else
            -- TODO
        end
    end


    -- eval_output_arg
    -- Given the AST for an output argument, evaluate it and return the
    -- value, as a string.
    function eval_output_arg(ast)
        -- TODO: WRITE THIS!!!
        assert(type(ast) == "table")
        local result
        if ast[1] == STRLIT_OUT then
            local str = ast[2]
            result = str:sub(2, str:len()-1)
        elseif ast[1] == EOL_OUT then
            result = "\n"
        elseif ast[1] == CHAR_CALL then
        else -- expression
            val = eval_expr(ast)
            result = numToStr(val)
        end

        return result
    end

    -- eval_expr
    -- Given the AST for an expression, evaluate it and return the
    -- value, as a number.
    function eval_expr(ast)
        -- TODO: WRITE THIS!!!
        assert(type(ast) == "table")
        local result
        if ast[1] == NUMLIT_VAL then
            result = strToNum(ast[2])
        else
            -- unimplemented
            result = 42
        end
        return result
    end


    -- Body of function interp
    interp_program(ast)
    return state
end


-- *********************************************************************
-- Module Table Return
-- *********************************************************************


return interpit