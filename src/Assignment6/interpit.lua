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
local function astToStr(x)
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
		print(astToStr(ast))
        if ast[1] == EMPTY_STMT then
            -- Do nothing
        elseif ast[1] == OUTPUT_STMT then
			print("at output")
			print(astToStr(ast))
            for i = 2, #ast do
                local str = eval_output_arg(ast[i])
                util.output(str)
            end
        elseif ast[1] == FUNC_DEF then
            local func = ast[2]
            local funcbody = ast[3]
            state.f[func] = funcbody
        elseif ast[1] == FUNC_CALL then
			print("at func_call")
			print(astToStr(ast))
            local func = ast[2]
            local funcbody = state.f[func]
            if funcbody == nil then
                funcbody = { PROGRAM }
            end
            interp_program(funcbody)
        elseif ast[1] == ASSN_STMT then
			local type = ast[2][1]
			local name = ast[2][2]
			local val = eval_expr(ast[3])
			if type == ARRAY_VAR then
				state.a[name] = state.a[name] or {}
				local index = eval_expr(ast[2][3])
				state.a[name][index] = val
			else
				state.v[name] = val
			end
		elseif ast[1] == IF_STMT then
			for i = 2, #ast, 2 do
				if ast[i][1] == 1 then
					interp_program(ast[i])
				else
					local cond = eval_expr(ast[i])
					if cond ~= 0 then
						interp_program(ast[i+1])
						break
					end
				end
			end
		elseif ast[1] == WHILE_LOOP then
			while eval_expr(ast[2]) ~= 0 do
				interp_program(ast[3])
			end
		elseif ast[1] == RETURN_STMT then
			print(astToStr(ast))
			local value = eval_expr(ast[2])
			state.v["return"] = value
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
			print(astToStr(ast[2]))
			local value = eval_expr(ast[2])
			print(value)
            if value >= 0 and value <= 255 then
                result = string.char(value)
            else
                result = string.char(0)
            end
		elseif ast[1] == ARRAY_VAR then
			local name = ast[2]
			local index = eval_expr(ast[3])
			if state.a[name] then
				local value = state.a[name][index]

                if type(value) == "number" then
                    result = numToStr(value)
                elseif value == nil then
                    result = "0"
                else
                    result = tostring(value)
                end
            else
                result = "0"
            end
        else -- expression
            local val = eval_expr(ast)
            result = numToStr(val)
        end
    	return result
	end

    -- eval_expr
    -- Given the AST for an expression, evaluate it and return the
    -- value, as a number.
    function eval_expr(ast)
        assert(type(ast) == "table")
        local result
        if ast[1] == NUMLIT_VAL then
            result = strToNum(ast[2])
		elseif ast[1] == BOOLLIT_VAL then
			result = boolToInt(ast[2] == "true")
		elseif ast[1] == SIMPLE_VAR then
			if state.v[ast[2]] == nil then
				result = strToNum(ast[2])
			else
				result = state.v[ast[2]]
			end
		elseif ast[1] == ARRAY_VAR then
			local arrayName = ast[2]
			local index = eval_expr(ast[3])
			
			state.a[arrayName] = state.a[arrayName] or {}
			result = state.a[arrayName][index] or 0
		elseif ast[1] == INPUT_CALL then
			result = strToNum(util.input())
		elseif type(ast[1]) == "table" then
			print("at 325")
			print(astToStr(ast))
			if ast[1][1] == BIN_OP then
				print("at bin")
				print(astToStr(ast))
				local op = ast[1][2]
				local leftvalue = eval_expr(ast[2])
				local rightvalue = eval_expr(ast[3])
				
				if op == "+" then
					result = leftvalue + rightvalue
				elseif op == "-" then
					result = leftvalue - rightvalue
				elseif op == "*" then
					result = leftvalue * rightvalue
				elseif op == "/" then
					if rightvalue == 0 then
					
						result = 0
					else
						if leftvalue / rightvalue >= 0 then
							result = math.floor(leftvalue / rightvalue)
						else
							result = math.ceil(leftvalue / rightvalue)
						end
					end
					elseif op == "%" then
						if rightvalue == 0 then
							result = 0
						else
							result = leftvalue % rightvalue
						end
					elseif op == "==" then
						result = boolToInt(leftvalue == rightvalue)
					elseif op == ">=" then
						result = boolToInt(leftvalue >= rightvalue)
					elseif op == "<" then
						result = boolToInt(leftvalue < rightvalue)
					elseif op == ">" then
						result = boolToInt(leftvalue > rightvalue)
					elseif op == "<=" then
						result = boolToInt(leftvalue <= rightvalue)
					elseif op == "!=" then
						result = boolToInt(leftvalue ~= rightvalue)
					elseif op == "and" then
						result = boolToInt(leftvalue ~= 0 and rightvalue ~= 0)
					elseif op == "or" then
						result = boolToInt( leftvalue ~= 0 or rightvalue ~= 0)
					else
						result = 0
					end
				elseif ast[1][1] == UN_OP then
					local op = ast[1][2]
					local value = eval_expr(ast[2])
					if op == "-" then
						result = -value
					elseif op == "not" then
						value = eval_expr(ast[2])
						result = boolToInt(value == 0)
					else
						result = value
					end
				elseif ast[1] == RAND_CALL then
					local num = eval_expr(ast[2])
					result = util.random(num)
				elseif ast[1] == FUNC_CALL then
					local funcname = ast[2]
					local funcbody = state.f[funcname]
					if funcbody == nil then
						funcbody = { PROGRAM }
					end
					interp_program (funcbody)
					if state.v["return"] ~= nil then
						result = state.v["return"]
					else
						result = 0
					end
			else
				print("unkown: " .. ast[1])
				print("type: " .. type(ast[1]))
				result = ast[1]
			end
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