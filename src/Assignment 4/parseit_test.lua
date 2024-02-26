#!/usr/bin/env lua
-- parseit_test.lua
-- Glenn G. Chappell
-- 2024-02-20
--
-- For CS 331 Spring 2024
-- Test Program for Module parseit
-- Used in Assignment 4, Exercise A

parseit = require "parseit"  -- Import parseit module


-- *********************************************
-- * YOU MAY WISH TO CHANGE THE FOLLOWING LINE *
-- *********************************************

EXIT_ON_FIRST_FAILURE = true
-- If EXIT_ON_FIRST_FAILURE is true, then this program exits after the
-- first failing test. If it is false, then this program executes all
-- tests, reporting success/failure for each.


-- *********************************************************************
-- Testing Package
-- *********************************************************************


tester = {}
tester.countTests = 0
tester.countPasses = 0

function tester.test(self, success, testName)
    self.countTests = self.countTests+1
    io.write("    Test: " .. testName .. " - ")
    if success then
        self.countPasses = self.countPasses+1
        io.write("passed")
    else
        io.write("********** FAILED **********")
    end
    io.write("\n")
end

function tester.allPassed(self)
    return self.countPasses == self.countTests
end


-- *********************************************************************
-- Utility Functions
-- *********************************************************************


-- terminate
-- Called to end the program. Currently simply ends. To make the program
-- prompt the user and wait for the user to press ENTER, uncomment the
-- commented-out lines in the function body. The parameter is the
-- program's return value.
function terminate(status)
    -- Uncomment to following to wait for the user before terminating.
    --io.write("\nPress ENTER to quit ")
    --io.read("*l")

    os.exit(status)
end


function failExit()
    if EXIT_ON_FIRST_FAILURE then
        io.write("**************************************************\n")
        io.write("* This test program is configured to exit after  *\n")
        io.write("* the first failing test. To make it execute all *\n")
        io.write("* tests, reporting success/failure for each, set *\n")
        io.write("* variable                                       *\n")
        io.write("*                                                *\n")
        io.write("*   EXIT_ON_FIRST_FAILURE                        *\n")
        io.write("*                                                *\n")
        io.write("* to false, near the start of the test program.  *\n")
        io.write("**************************************************\n")

        -- Terminate program, signaling error
        terminate(1)
    end
end


function endMessage(passed)
    if passed then
        io.write("All tests successful\n")
    else
        io.write("Tests ********** UNSUCCESSFUL **********\n")
        io.write("\n")
        io.write("**************************************************\n")
        io.write("* This test program is configured to execute all *\n")
        io.write("* tests, reporting success/failure for each. To  *\n")
        io.write("* make it exit after the first failing test, set *\n")
        io.write("* variable                                       *\n")
        io.write("*                                                *\n")
        io.write("*   EXIT_ON_FIRST_FAILURE                        *\n")
        io.write("*                                                *\n")
        io.write("* to true, near the start of the test program.   *\n")
        io.write("**************************************************\n")
    end
end


-- printValue
-- Given a value, print it in (roughly) Lua literal notation if it is
-- nil, number, string, boolean, or table -- calling this function
-- recursively for table keys and values. For other types, print an
-- indication of the type. The second argument, if passed, is max_items:
-- the maximum number of items in a table to print.
function printValue(...)
    assert(select("#", ...) == 1 or select("#", ...) == 2,
           "printValue: must pass 1 or 2 arguments")
    local x, max_items = select(1, ...)  -- Get args (may be nil)
    if type(max_items) ~= "nil" and type(max_items) ~= "number" then
        error("printValue: max_items must be a number")
    end

    if type(x) == "nil" then
        io.write("nil")
    elseif type(x) == "number" then
        io.write(x)
    elseif type(x) == "string" then
        io.write('"'..x..'"')
    elseif type(x) == "boolean" then
        if x then
            io.write("true")
        else
            io.write("false")
        end
    elseif type(x) ~= "table" then
        io.write('<'..type(x)..'>')
    else  -- type is "table"
        io.write("{")
        local first = true  -- First iteration of loop?
        local key_count, unprinted_count = 0, 0
        for k, v in pairs(x) do
            key_count = key_count + 1
            if max_items ~= nil and key_count > max_items then
                unprinted_count = unprinted_count + 1
            else
                if first then
                    first = false
                else
                    io.write(",")
                end
                io.write(" [")
                printValue(k, max_items)
                io.write("]=")
                printValue(v, max_items)
            end
        end
        if unprinted_count > 0 then
            if first then
                first = false
            else
                io.write(",")
            end
            io.write(" <<"..unprinted_count)
            if key_count - unprinted_count > 0 then
                io.write(" more")
            end
            if unprinted_count == 1 then
                io.write(" item>>")
            else
                io.write(" items>>")
            end
        end
        io.write(" }")
    end
end


-- printArray
-- Like printValue, but prints top-level tables as arrays.
-- Uses printValue.
-- The second argument, if passed, is max_items: the maximum number of
-- items in a table to print.
function printArray(...)
    assert(select("#", ...) == 1 or select("#", ...) == 2,
           "printArray: must pass 1 or 2 arguments")
    local x, max_items = select(1, ...)  -- Get args (may be nil)
    if type(max_items) ~= "nil" and type(max_items) ~= "number" then
        error("printArray: max_items must be a number")
    end

    if type(x) ~= "table" then
        printValue(x, max_items)
    else
        io.write("{")
        local first = true  -- First iteration of loop?
        local key_count, unprinted_count = 0, 0
        for k, v in ipairs(x) do
            key_count = key_count + 1
            if max_items ~= nil and key_count > max_items then
                unprinted_count = unprinted_count + 1
            else
                if first then
                    first = false
                else
                    io.write(",")
                end
                io.write(" ")
                printValue(v, max_items)
            end
        end
        if unprinted_count > 0 then
            if first then
                first = false
            else
                io.write(",")
            end
            io.write(" <<"..unprinted_count)
            if key_count - unprinted_count > 0 then
                io.write(" more")
            end
            if unprinted_count == 1 then
                io.write(" item>>")
            else
                io.write(" items>>")
            end
        end
        io.write(" }")
    end
end


-- equal
-- Compare equality of two values. Returns false if types are different.
-- Uses "==" on non-table values. For tables, recurses for the value
-- associated with each key.
function equal(...)
    assert(select("#", ...) == 2,
           "equal: must pass exactly 2 arguments")
    local x1, x2 = select(1, ...)  -- Get args (may be nil)

    local type1 = type(x1)
    if type1 ~= type(x2) then
        return false
    end

    if type1 ~= "table" then
       return x1 == x2
    end

    -- Get number of keys in x1 & check values in x1, x2 are equal
    local x1numkeys = 0
    for k, v in pairs(x1) do
        x1numkeys = x1numkeys + 1
        if not equal(v, x2[k]) then
            return false
        end
    end

    -- Check number of keys in x1, x2 same
    local x2numkeys = 0
    for k, v in pairs(x2) do
        x2numkeys = x2numkeys + 1
    end
    return x1numkeys == x2numkeys
end


-- getCoroutineValues
-- Given coroutine f, returns array of all values yielded by f when
-- passed param as its parameter, in the order the values are yielded.
function getCoroutineValues(f, param)
    assert(type(f)=="function",
           "getCoroutineValues: f is not a function")

    local covals = {}  -- Array of values yielded by coroutine f
    local co = coroutine.create(f)
    local ok, value = coroutine.resume(co, param)

    while (coroutine.status(co) ~= "dead") do
        table.insert(covals, value)
        ok, value = coroutine.resume(co)
    end

    -- Error in coroutine?
    if not ok then
        io.write("*** getCoroutineValues: error in coroutine:\n")
        io.write(value.."\n")  -- Print error trace
        terminate(1)
    end

    -- Return array of values
    return covals
end


-- *********************************************************************
-- Definitions for This Test Program
-- *********************************************************************


-- Symbolic Constants for AST
-- Names differ from those in assignment, to avoid interference.
local PROGRAMx     = 1
local EMPTYxSTMT   = 2
local OUTPUTxSTMT  = 3
local RETURNxSTMT  = 4
local ASSNxSTMT    = 5
local FUNCxCALL    = 6
local FUNCxDEF     = 7
local IFxSTMT      = 8
local WHILExLOOP   = 9
local STRLITxOUT   = 10
local EOLxOUT      = 11
local CHARxCALL    = 12
local BINxOP       = 13
local UNxOP        = 14
local NUMLITxVAL   = 15
local BOOLLITxVAL  = 16
local INPUTxCALL   = 17
local RANDxCALL    = 18
local SIMPLExVAR   = 19
local ARRAYxVAR    = 20


-- String forms of symbolic constants
-- Used by printAST_parseit
symbolNames = {
  [1]="PROGRAM",
  [2]="EMPTY_STMT",
  [3]="OUTPUT_STMT",
  [4]="RETURN_STMT",
  [5]="ASSN_STMT",
  [6]="FUNC_CALL",
  [7]="FUNC_DEF",
  [8]="IF_STMT",
  [9]="WHILE_LOOP",
  [10]="STRLIT_OUT",
  [11]="EOL_OUT",
  [12]="CHAR_CALL",
  [13]="BIN_OP",
  [14]="UN_OP",
  [15]="NUMLIT_VAL",
  [16]="BOOLLIT_VAL",
  [17]="INPUT_CALL",
  [18]="RAND_CALL",
  [19]="SIMPLE_VAR",
  [20]="ARRAY_VAR",
}


-- printAST_parseit
-- Write an AST, in (roughly) Lua form, with numbers replaced by the
-- symbolic constants used in parseit, where possible.
-- See the Assignment description for the AST Specification.
function printAST_parseit(...)
    if select("#", ...) ~= 1 then
        error("printAST_parseit: must pass exactly 1 argument")
    end
    local x = select(1, ...)  -- Get argument (which may be nil)

    if type(x) == "nil" then
        io.write("nil")
    elseif type(x) == "number" then
        if symbolNames[x] then
            io.write(symbolNames[x])
        else
            io.write("<ERROR: Unknown constant: "..x..">")
        end
    elseif type(x) == "string" then
        io.write('"'..x..'"')
    elseif type(x) == "boolean" then
        if x then
            io.write("true")
        else
            io.write("false")
        end
    elseif type(x) ~= "table" then
        io.write('<'..type(x)..'>')
    else  -- type is "table"
        io.write("{ ")
        local first = true  -- First iteration of loop?
        local maxk = 0
        for k, v in ipairs(x) do
            if first then
                first = false
            else
                io.write(", ")
            end
            maxk = k
            printAST_parseit(v)
        end
        for k, v in pairs(x) do
            if type(k) ~= "number"
              or k ~= math.floor(k)
              or (k < 1 and k > maxk) then
                if first then
                    first = false
                else
                    io.write(", ")
                end
                io.write("[")
                printAST_parseit(k)
                io.write("]=")
                printAST_parseit(v)
            end
        end
        io.write(" }")
    end
end


-- bool2Str
-- Given boolean, return string representing it: "true" or "false".
function bool2Str(b)
    if b then
        return "true"
    else
        return "false"
    end
end


-- checkParse
-- Given tester object, input string ("program"), expected output values
-- from parser (good, AST), and string giving the name of the test. Do
-- test & print result. If test fails and EXIT_ON_FIRST_FAILURE is true,
-- then print detailed results and exit program.
function checkParse(t, prog,
                    expectedGood, expectedDone, expectedAST,
                    testName)
    local actualGood, actualDone, actualAST = parseit.parse(prog)
    local sameGood = (expectedGood == actualGood)
    local sameDone = (expectedDone == actualDone)
    local sameAST = true
    if sameGood and expectedGood and sameDone and expectedDone then
        sameAST = equal(expectedAST, actualAST)
    end
    local success = sameGood and sameDone and sameAST
    t:test(success, testName)

    if success or not EXIT_ON_FIRST_FAILURE then
        return
    end

    io.write("\n")
    io.write("Input for the last test above:\n")
    io.write('"'..prog..'"\n')
    io.write("\n")
    io.write("Expected parser 'good' return value: ")
    io.write(bool2Str(expectedGood).."\n")
    io.write("Actual parser 'good' return value: ")
    io.write(bool2Str(actualGood).."\n")
    io.write("Expected parser 'done' return value: ")
    io.write(bool2Str(expectedDone).."\n")
    io.write("Actual parser 'done' return value: ")
    io.write(bool2Str(actualDone).."\n")
    if not sameAST then
        io.write("\n")
        io.write("Expected AST:\n")
        printAST_parseit(expectedAST)
        io.write("\n")
        io.write("\n")
        io.write("Returned AST:\n")
        printAST_parseit(actualAST)
        io.write("\n")
    end
    io.write("\n")
    failExit()
end


-- *********************************************************************
-- Test Suite Functions
-- *********************************************************************


function test_simple(t)
    io.write("Test Suite: simple cases\n")

    checkParse(t, "", true, true, {PROGRAMx},
      "Empty program")
    checkParse(t, "return", false, true, nil,
      "Bad program: Keyword only #1")
    checkParse(t, "elseif", true, false, nil,
      "Bad program: Keyword only #2")
    checkParse(t, "else", true, false, nil,
      "Bad program: Keyword only #3")
    checkParse(t, "ab", false, true, nil,
      "Bad program: Identifier only")
    checkParse(t, "ab;", false, false, nil,
      "Bad program: Identifier + semicolon")
    checkParse(t, "123", true, false, nil,
      "Bad program: NumericLiteral only")
    checkParse(t, "123;", true, false, nil,
      "Bad program: NumericLiteral + semicolon")
    checkParse(t, '"xyz"', true, false, nil,
      "Bad program: StringLiteral only")
    checkParse(t, "<=", true, false, nil,
      "Bad program: Operator only")
    checkParse(t, "{", true, false, nil,
      "Bad program: Punctuation only")
    checkParse(t, "\a", true, false, nil,
      "Bad program: Malformed only #1 (bad character)")
    checkParse(t, '"', true, false, nil,
      "bad program: malformed only #2 (bad string)")
end


function test_output_stmt_no_expr(t)
    io.write("Test Suite: output statements - no expressions\n")

    checkParse(t, "output();", true, true,
      {PROGRAMx,{OUTPUTxSTMT}},
      "Output statement: no args")
    checkParse(t, "output();output();output();", true, true,
      {PROGRAMx,{OUTPUTxSTMT},{OUTPUTxSTMT},{OUTPUTxSTMT}},
      "3 output statements")
    checkParse(t, "output(\"abc\");", true, true,
      {PROGRAMx,{OUTPUTxSTMT,{STRLITxOUT,"\"abc\""}}},
      "Output statement: StringLiteral")
    checkParse(t, "output(\"a\",\"b\",\"c\",\"d\",\"e\");", true, true,
      {PROGRAMx,{OUTPUTxSTMT,{STRLITxOUT,"\"a\""},{STRLITxOUT,"\"b\""},
        {STRLITxOUT,"\"c\""},{STRLITxOUT,"\"d\""},
        {STRLITxOUT,"\"e\""}}},
      "Output statement: many StringLiterals")
    checkParse(t, "output()", false, true, nil,
      "Bad output statement: no semicolon")
    checkParse(t, "output", false, true, nil,
      "Bad output statement: no parens, no arguments, no semicolon")
    checkParse(t, "output \"a\";", false, false, nil,
      "Bad output statement: no parens")
    checkParse(t, "output\"a\");", false, false, nil,
      "Bad output statement: no opening paren")
    checkParse(t, "output(\"a\";", false, false, nil,
      "Bad output statement: no closing paren")
    checkParse(t, "output(if);", false, false, nil,
      "Bad output statement: keyword #1")
    checkParse(t, "output(output);", false, false, nil,
      "Bad output statement: keyword #2")
    checkParse(t, "output(\"a\"\"b\");", false, false, nil,
      "Bad output statement: missing comma")
    checkParse(t, "output(,\"a\");", false, false, nil,
      "Bad output statement: comma without preceding argument")
    checkParse(t, "output(\"a\",);", false, false, nil,
      "Bad output statement: comma without following argument")
    checkParse(t, "output(,);", false, false, nil,
      "Bad output statement: comma alone")
    checkParse(t, "output(\"a\",,\"b\");", false, false, nil,
      "Bad output statement: extra comma")
    checkParse(t, "output(\"a\")else;", false, false, nil,
      "Bad output statement: output followed by else, semicolon")
    checkParse(t, "output(\"a\");else", true, false, nil,
      "Bad output statement: output followed by semicolon, else")
    checkParse(t, "\"a\";", true, false, nil,
      "Bad program: (no output) string only")
    checkParse(t, "(\"a\");", true, false, nil,
      "Bad program: (no output) string in parens only")
end


function test_function_call_stmt(t)
    io.write("Test Suite: Function call statements\n")

    checkParse(t, "ff();", true, true,
      {PROGRAMx,{FUNCxCALL,"ff"}},
      "Function call statement #1")
    checkParse(t, "fffffffffffffffffffffffffffffffff();", true, true,
      {PROGRAMx,{FUNCxCALL,"fffffffffffffffffffffffffffffffff"}},
      "Function call statement #2")
    checkParse(t, "ff();gg();", true, true,
      {PROGRAMx,{FUNCxCALL,"ff"},{FUNCxCALL,"gg"}},
      "Two function call statements")
    checkParse(t, "ff()", false, true, nil,
      "Bad function call statement: no semicolon")
    checkParse(t, "ff);", false, false, nil,
      "Bad function call statement: no left paren")
    checkParse(t, "ff(;", false, false, nil,
      "Bad function call statement: no right paren")
    checkParse(t, "ff(();", false, false, nil,
      "Bad function call statement: extra left paren")
    checkParse(t, "ff());", false, false, nil,
      "Bad function call statement: extra right paren")
    checkParse(t, "ff()();", false, false, nil,
      "Bad function call statement: extra pair of parens")
    checkParse(t, "ff gg();", false, false, nil,
      "Bad function call statement: extra name")
    checkParse(t, "(ff)();", true, false, nil,
      "Bad function call statement: parentheses around name")
    checkParse(t, "ff(a);", false, false, nil,
      "Bad function call statement: argument - Identfier arg")
    checkParse(t, "ff(\"abc\");", false, false, nil,
      "Bad function call statement: argument - StringLiteral arg")
    checkParse(t, "ff(2);", false, false, nil,
      "Bad function call statement: argument - NumericLiteral arg")
end


function test_func_def_no_expr(t)
    io.write("Test Suite: function definitions - no expressions\n")

    checkParse(t, "def s(){}", true, true,
      {PROGRAMx,{FUNCxDEF,"s",{PROGRAMx}}},
      "Function definition: empty body")
    checkParse(t, "def(){}", false, false, nil,
      "Bad function definition: missing name")
    checkParse(t, "def{}", false, false, nil,
      "Bad function definition: missing name & parens")
    checkParse(t, "def &s(){}", false, false, nil,
      "Bad function definition: ampersand before name")
    checkParse(t, "def s{}", false, false, nil,
      "Bad function definition: no parens")
    checkParse(t, "def s()", false, true, nil,
      "Bad function definition: no braces")
    checkParse(t, "def s()end", false, false, nil,
      "Bad function definition: end instead of braces")
    checkParse(t, "def s()){}", false, false, nil,
      "Bad function definition: extra right paren")
    checkParse(t, "def (s)(){}", false, false, nil,
      "Bad function definition: name in parentheses")
    checkParse(t, "def s(){output(\"abc\");}", true, true,
      {PROGRAMx,{FUNCxDEF,"s",{PROGRAMx,{OUTPUTxSTMT,
        {STRLITxOUT,"\"abc\""}}}}},
      "Function definition: 1-statement body #1")
    checkParse(t, "def s(){output(\"x\");}", true, true,
      {PROGRAMx,{FUNCxDEF,"s",{PROGRAMx,{OUTPUTxSTMT,
        {STRLITxOUT,"\"x\""}}}}},
      "Function definition: 1-statment body #2")
    checkParse(t, "def s(){output();output();}", true, true,
      {PROGRAMx,{FUNCxDEF,"s",{PROGRAMx,{OUTPUTxSTMT},
        {OUTPUTxSTMT}}}},
      "Function definition: 2-statment body")
    checkParse(t, "def sss(){output();output();output();}",
      true, true,
      {PROGRAMx,{FUNCxDEF,"sss",{PROGRAMx,{OUTPUTxSTMT},
        {OUTPUTxSTMT},{OUTPUTxSTMT}}}},
      "Function definition: longer body")
    checkParse(t, "def s(){def t(){def u(){output();}"
     .." def v(){output();}}}", true, true,
      {PROGRAMx,{FUNCxDEF,"s",{PROGRAMx,{FUNCxDEF,"t",{PROGRAMx,
        {FUNCxDEF,"u",{PROGRAMx,{OUTPUTxSTMT}}},{FUNCxDEF,
        "v",{PROGRAMx,{OUTPUTxSTMT}}}}}}}},
      "Function definition: nested function definitions")
end

function test_while_loop_simple_expr(t)
    io.write("Test Suite: while loops - simple expressions only\n")

    checkParse(t, "while(true){}", true, true,
      {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},{PROGRAMx}}},
      "While loop: true, empty")
    checkParse(t, "while(true){output();}", true, true,
      {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},{PROGRAMx,
        {OUTPUTxSTMT}}}},
      "While loop: true, 1 stmt in body")
    checkParse(t, "while(true){output();output();output();output();"
     .."output();output();output();output();output();output();}", true,
     true,
      {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},{PROGRAMx,
        {OUTPUTxSTMT},{OUTPUTxSTMT},{OUTPUTxSTMT},{OUTPUTxSTMT},
        {OUTPUTxSTMT},{OUTPUTxSTMT},{OUTPUTxSTMT},{OUTPUTxSTMT},
        {OUTPUTxSTMT},{OUTPUTxSTMT}}}},
      "While loop: true, longer statement list")
    checkParse(t, "while(true){while(true){while(true)"
     .."{while(true){while(true){while(true){while(true)"
     .."{while(true){while(true){while(true){while(true)"
     .."{while(true){while(true){while(true){}}}}}}}}}}}}}}",
     true, true,
      {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},
        {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},
        {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},
        {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},
        {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},
        {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},
        {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},
        {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},
        {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},
        {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},
        {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},
        {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},
        {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},
        {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},
        {PROGRAMx}}}}}}}}}}}}}}}}}}}}}}}}}}}}},
      "While loop: true, nested")

    checkParse(t, "while(){}", false, false, nil,
      "Bad while loop: no condition")
    checkParse(t, "while{}", false, false, nil,
      "Bad while loop: no parentheses")
    checkParse(t, "while(true)", false, true, nil,
      "Bad while loop: no braces")
    checkParse(t, "while while(true){}", false, false, nil,
      "Bad while loop: extra while")
    checkParse(t, "while(true)(true){}", false, false, nil,
      "Bad while loop: extra condition")
    checkParse(t, "while(true){{}", false, false, nil,
      "Bad while loop: extra left brace")
    checkParse(t, "while(true){}}", true, false, nil,
      "Bad while loop: extra right brace")
end


function test_if_stmt_simple_expr(t)
    io.write("Test Suite: if statements - simple expressions only\n")

    checkParse(t, "if(true){}", true, true,
      {PROGRAMx,{IFxSTMT,{BOOLLITxVAL,"true"},{PROGRAMx}}},
      "If statement: empty stmt list")
    checkParse(t, "if(true){output();}", true, true,
      {PROGRAMx,{IFxSTMT,{BOOLLITxVAL,"true"},{PROGRAMx,
      {OUTPUTxSTMT}}}},
      "If statement: one stmt in body")
    checkParse(t, "if(true){}else{}", true,
      true,
      {PROGRAMx,{IFxSTMT,{BOOLLITxVAL,"true"},{PROGRAMx},
      {PROGRAMx}}},
      "If statement: else")
    checkParse(t, "if(true){}elseif(true){}", true, true,
      {PROGRAMx,{IFxSTMT,{BOOLLITxVAL,"true"},{PROGRAMx},
        {BOOLLITxVAL,"true"},{PROGRAMx}}},
      "If statement: elseif, else")
    checkParse(t, "if(true){output();}elseif(true){output();output();}"
      .."elseif(true){output();output();output();}elseif(true){"
      .."output();output();output();output();}elseif(true){output();"
      .."output();output();output();output();}", true, true,
      {PROGRAMx,{IFxSTMT,{BOOLLITxVAL,"true"},{PROGRAMx,{OUTPUTxSTMT}},
        {BOOLLITxVAL,"true"},{PROGRAMx,{OUTPUTxSTMT},{OUTPUTxSTMT}},
        {BOOLLITxVAL,"true"},{PROGRAMx,{OUTPUTxSTMT},
        {OUTPUTxSTMT},{OUTPUTxSTMT}},
        {BOOLLITxVAL,"true"},{PROGRAMx,{OUTPUTxSTMT},{OUTPUTxSTMT},
        {OUTPUTxSTMT},{OUTPUTxSTMT}},
        {BOOLLITxVAL,"true"},{PROGRAMx,{OUTPUTxSTMT},{OUTPUTxSTMT},
        {OUTPUTxSTMT},{OUTPUTxSTMT},{OUTPUTxSTMT}}}},
      "If statement: multiple elseif, no else")
    checkParse(t, "if(true){output();}elseif(true){output();output();}"
      .."elseif(true){output();output();output();}elseif(true){"
      .."output();output();output();output();}elseif(true){output();"
      .."output();output();output();output();}else{output();output();"
      .."output();output();output();output();}", true, true,
      {PROGRAMx,{IFxSTMT,{BOOLLITxVAL,"true"},{PROGRAMx,{OUTPUTxSTMT}},
        {BOOLLITxVAL,"true"},{PROGRAMx,{OUTPUTxSTMT},{OUTPUTxSTMT}},
        {BOOLLITxVAL,"true"},{PROGRAMx,{OUTPUTxSTMT},
        {OUTPUTxSTMT},{OUTPUTxSTMT}},
        {BOOLLITxVAL,"true"},{PROGRAMx,{OUTPUTxSTMT},{OUTPUTxSTMT},
        {OUTPUTxSTMT},{OUTPUTxSTMT}},
        {BOOLLITxVAL,"true"},{PROGRAMx,{OUTPUTxSTMT},{OUTPUTxSTMT},
        {OUTPUTxSTMT},{OUTPUTxSTMT},{OUTPUTxSTMT}},{PROGRAMx,
        {OUTPUTxSTMT},{OUTPUTxSTMT},{OUTPUTxSTMT},{OUTPUTxSTMT},
        {OUTPUTxSTMT},{OUTPUTxSTMT}}}},
      "If statement: multiple elseif, else")
    checkParse(t, "if(true){if(true){output();}else{output();}}"
      .."elseif(true){if(true){output();}else{output();}}else"
      .."{if(true){output();}else{output();}}", true, true,
        {PROGRAMx,{IFxSTMT,{BOOLLITxVAL,"true"},{PROGRAMx,{IFxSTMT,
          {BOOLLITxVAL,"true"},{PROGRAMx,{OUTPUTxSTMT}},{PROGRAMx,
          {OUTPUTxSTMT}}}},{BOOLLITxVAL,"true"},{PROGRAMx,{IFxSTMT,
          {BOOLLITxVAL,"true"},{PROGRAMx,{OUTPUTxSTMT}},{PROGRAMx,
          {OUTPUTxSTMT}}}},{PROGRAMx,{IFxSTMT,{BOOLLITxVAL,"true"},
          {PROGRAMx,{OUTPUTxSTMT}},{PROGRAMx,{OUTPUTxSTMT}}}}}},
      "If statement: nested #1")
    checkParse(t, "if(true){if(true){if(true){if(true)"
      .."{if(true){if(true){if(true){output();}}}}}}}", true, true,
      {PROGRAMx,{IFxSTMT,{BOOLLITxVAL,"true"},{PROGRAMx,{IFxSTMT,
        {BOOLLITxVAL,"true"},{PROGRAMx,{IFxSTMT,{BOOLLITxVAL,"true"},
        {PROGRAMx,{IFxSTMT,{BOOLLITxVAL,"true"},{PROGRAMx,{IFxSTMT,
        {BOOLLITxVAL,"true"},{PROGRAMx,{IFxSTMT,{BOOLLITxVAL,"true"},
        {PROGRAMx,{IFxSTMT,{BOOLLITxVAL,"true"},{PROGRAMx,
        {OUTPUTxSTMT}}}}}}}}}}}}}}}},
      "If statement: nested #2")
    checkParse(t, "while(true){if(true){while(true){}}"
      .."elseif(true){while(true){if(true){}elseif(true)"
      .."{while(true){}}else{while(true){}}}}}",
      true, true,
      {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},{PROGRAMx,{IFxSTMT,
        {BOOLLITxVAL,"true"},{PROGRAMx,{WHILExLOOP,
        {BOOLLITxVAL,"true"},{PROGRAMx}}},{BOOLLITxVAL,"true"},
        {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},{PROGRAMx,{IFxSTMT,
        {BOOLLITxVAL,"true"},{PROGRAMx},{BOOLLITxVAL,"true"},
        {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},{PROGRAMx}}},
        {PROGRAMx,{WHILExLOOP,{BOOLLITxVAL,"true"},
        {PROGRAMx}}}}}}}}}}},
      "If statement: nested while & if")

    checkParse(t, "if(){output();}", false, false, nil,
      "Bad if statement: no condition")
    checkParse(t, "if{output();}", false, false, nil,
      "Bad if statement: no parentheses")
    checkParse(t, "if(true)output();}", false, false, nil,
      "Bad if statement: no open brace")
    checkParse(t, "if output();}", false, false, nil,
      "Bad if statement: no condition or open brace")
    checkParse(t, "if(true){", false, true, nil,
      "Bad if statement: no close brace")
    checkParse(t, "if(true)(true){}", false, false, nil,
      "Bad if statement: 2 expressions")
    checkParse(t, "if(true){}else{}elseif(true){}",
      true, false, nil,
      "Bad if statement: else before elseif")
    checkParse(t, "if(true){}}", true, false, nil,
      "Bad if statement: extra right brace")
end


function test_assn_stmt(t)
    io.write("Test Suite: assignment statements - simple expressions\n")

    checkParse(t, "abc=123;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"abc"},{NUMLITxVAL,"123"}}},
      "Assignment statement: NumericLiteral")
    checkParse(t, "abc=xyz;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR, "abc"},{SIMPLExVAR,"xyz"}}},
      "Assignment statement: identifier")
    checkParse(t, "abc[1]=xyz;", true, true,
      {PROGRAMx,{ASSNxSTMT,{ARRAYxVAR,"abc",{NUMLITxVAL,"1"}},
        {SIMPLExVAR,"xyz"}}},
      "Assignment statement: array ref = ...")
    checkParse(t, "abc=true;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR, "abc"},{BOOLLITxVAL,"true"}}},
      "Assignment statement: boolean literal Keyword: true")
    checkParse(t, "abc=false;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR, "abc"},{BOOLLITxVAL,"false"}}},
      "Assignment statement: boolean literal Keyword: false")
    checkParse(t, "=123;", true, false, nil,
      "Bad assignment statement: missing LHS")
    checkParse(t, "123=123;", true, false, nil,
      "Bad assignment statement: LHS is NumericLiteral")
    checkParse(t, "else=123;", true, false, nil,
      "Bad assignment statement: LHS is Keyword")
    checkParse(t, "abc 123;", false, false, nil,
      "Bad assignment statement: missing assignment op")
    checkParse(t, "abc==123;", false, false, nil,
      "Bad assignment statement: assignment op replaced by equality")
    checkParse(t, "abc=123", false, true, nil,
      "Bad assignment statement: no semicolon")
    checkParse(t, "abc=;", false, false, nil,
      "Bad assignment statement: RHS is empty")
    checkParse(t, "abc=else;", false, false, nil,
      "Bad assignment statement: RHS is Keyword")
    checkParse(t, "abc=1 2;", false, false, nil,
      "Bad assignment statement: RHS is two NumericLiterals")
    checkParse(t, "abc=1 else;", false, false, nil,
      "Bad assignment statement: followed by else")

    checkParse(t, "x=foo();", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{FUNCxCALL,"foo"}}},
      "Simple expression: call")
    checkParse(t, "x=();", false, false, nil,
      "Bad expression: empty parentheses")
    checkParse(t, "x=1and 2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"and"},
        {NUMLITxVAL,"1"},{NUMLITxVAL,"2"}}}},
      "Simple expression: and")
    checkParse(t, "x=1 or 2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"or"},
        {NUMLITxVAL,"1"},{NUMLITxVAL,"2"}}}},
      "Simple expression: or")
    checkParse(t, "x=1 + 2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},
        {NUMLITxVAL,"1"},{NUMLITxVAL,"2"}}}},
      "Simple expression: binary + (numbers with space)")
    checkParse(t, "x=1+2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},
        {NUMLITxVAL,"1"},{NUMLITxVAL,"2"}}}},
      "Simple expression: binary + (numbers without space)")
    checkParse(t, "x=a+2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},
        {SIMPLExVAR,"a"},{NUMLITxVAL,"2"}}}},
      "Simple expression: binary + (var+number)")
    checkParse(t, "x=1+b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},
        {NUMLITxVAL,"1"},{SIMPLExVAR,"b"}}}},
      "Simple expression: binary + (number+var)")
    checkParse(t, "x=a+b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},
        {SIMPLExVAR,"a"},{SIMPLExVAR,"b"}}}},
      "Simple expression: binary + (vars)")
    checkParse(t, "x=1+;", false, false, nil,
      "Bad expression: end with +")
    checkParse(t, "x=1 - 2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"-"},
        {NUMLITxVAL,"1"},{NUMLITxVAL,"2"}}}},
      "Simple expression: binary - (numbers with space)")
    checkParse(t, "x=1-2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"-"},
        {NUMLITxVAL,"1"},{NUMLITxVAL,"2"}}}},
      "Simple expression: binary - (numbers without space)")
    checkParse(t, "x=1-;", false, false, nil,
      "Bad expression: end with -")
    checkParse(t, "x=1*2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"*"},
        {NUMLITxVAL,"1"},{NUMLITxVAL,"2"}}}},
      "Simple expression: * (numbers)")
    checkParse(t, "x=a*2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"*"},
        {SIMPLExVAR,"a"},{NUMLITxVAL,"2"}}}},
      "Simple expression: * (var*number)")
    checkParse(t, "x=1*b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"*"},
        {NUMLITxVAL,"1"},{SIMPLExVAR,"b"}}}},
      "Simple expression: * (number*var)")
    checkParse(t, "x=a*b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"*"},
        {SIMPLExVAR,"a"},{SIMPLExVAR,"b"}}}},
      "Simple expression: * (vars)")
    checkParse(t, "x=1*;", false, false, nil,
      "Bad expression: end with *")
    checkParse(t, "x=1/2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"/"},
        {NUMLITxVAL,"1"},{NUMLITxVAL,"2"}}}},
      "Simple expression: /")
    checkParse(t, "x=1/;", false, false, nil,
      "Bad expression: end with /")
    checkParse(t, "x=1%2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"%"},
        {NUMLITxVAL,"1"},{NUMLITxVAL,"2"}}}},
      "Simple expression: % #1")
    checkParse(t, "x=1%true;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"%"},
        {NUMLITxVAL,"1"},{BOOLLITxVAL,"true"}}}},
      "Simple expression: % #2")
    checkParse(t, "x=1%false;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"%"},
        {NUMLITxVAL,"1"},{BOOLLITxVAL,"false"}}}},
      "Simple expression: % #3")
    checkParse(t, "x=1%;", false, false, nil,
      "Bad expression: end with %")
    checkParse(t, "x=1==2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},
        {NUMLITxVAL,"1"},{NUMLITxVAL,"2"}}}},
      "Simple expression: == (numbers)")
    checkParse(t, "x=a==2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},
        {SIMPLExVAR,"a"},{NUMLITxVAL,"2"}}}},
      "Simple expression: == (var==number)")
    checkParse(t, "x=1==b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},
        {NUMLITxVAL,"1"},{SIMPLExVAR,"b"}}}},
      "Simple expression: == (number==var)")
    checkParse(t, "x=a==b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},
        {SIMPLExVAR,"a"},{SIMPLExVAR,"b"}}}},
      "Simple expression: == (vars)")
    checkParse(t, "x=1==;", false, false, nil,
      "Bad expression: end with ==")
    checkParse(t, "x=1!=2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"!="},
        {NUMLITxVAL,"1"},{NUMLITxVAL,"2"}}}},
      "Simple expression: !=")
    checkParse(t, "x=1!=;", false, false, nil,
      "Bad expression: end with !=")
    checkParse(t, "x=1<2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"<"},
        {NUMLITxVAL,"1"},{NUMLITxVAL,"2"}}}},
      "Simple expression: <")
    checkParse(t, "x=1<;", false, false, nil,
      "Bad expression: end with <")
    checkParse(t, "x=1<=2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"<="},
        {NUMLITxVAL,"1"},{NUMLITxVAL,"2"}}}},
      "Simple expression: <=")
    checkParse(t, "x=1<=;", false, false, nil,
      "Bad expression: end with <=")
    checkParse(t, "x=1>2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">"},
        {NUMLITxVAL,"1"},{NUMLITxVAL,"2"}}}},
      "Simple expression: >")
    checkParse(t, "x=1>;", false, false, nil,
      "Bad expression: end with >")
    checkParse(t, "x=1>=2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">="},
        {NUMLITxVAL,"1"},{NUMLITxVAL,"2"}}}},
      "Simple expression: >=")
    checkParse(t, "x=+a;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{UNxOP,"+"},{SIMPLExVAR,
        "a"}}}},
      "Simple expression: unary +")
    checkParse(t, "x=-a;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{UNxOP,"-"},{SIMPLExVAR,
        "a"}}}},
      "Simple expression: unary -")
    checkParse(t, "x=1>=;", false, false, nil,
      "Bad expression: end with >=")
    checkParse(t, "x=(1);", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{NUMLITxVAL,"1"}}},
      "Simple expression: parens (number)")
    checkParse(t, "x=(a);", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{SIMPLExVAR,"a"}}},
      "Simple expression: parens (var)")
    checkParse(t, "x=a[1];", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{ARRAYxVAR,"a",
        {NUMLITxVAL,"1"}}}},
      "Simple expression: array ref")
    checkParse(t, "x=(1;", false, false, nil,
      "Bad expression: no closing paren")
    checkParse(t, "x=a[1;", false, false, nil,
      "Bad expression: no closing bracket")
    checkParse(t, "x=a 1];", false, false, nil,
      "Bad expression: no opening bracket")
    checkParse(t, "x=a[];", false, false, nil,
      "Bad expression: empty brackets")
    checkParse(t, "x=(x);", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{SIMPLExVAR,"x"}}},
      "Simple expression: var in parens on RHS")
    checkParse(t, "(x)=x;", true, false, nil,
      "Bad expression: var in parens on LHS")
    checkParse(t, "x[1]=(x[1]);", true, true,
      {PROGRAMx,{ASSNxSTMT,{ARRAYxVAR,"x",{NUMLITxVAL,"1"}},
        {ARRAYxVAR,"x",{NUMLITxVAL,"1"}}}},
      "Simple expression: array ref in parens on RHS")
    checkParse(t, "(x[1])=x[1];", true, false, nil,
      "Bad expression: array ref in parens on LHS")

    checkParse(t, "x=f()();", false, false, nil,
      "Bad expression: call function call")
    checkParse(t, "x=3();", false, false, nil,
      "Bad expression: call number")
    checkParse(t, "x=true();", false, false, nil,
      "Bad expression: call boolean")
    checkParse(t, "x=(x)();", false, false, nil,
      "Bad expression: call with parentheses around ID")
end


function test_return_stmt(t)
    io.write("Test Suite: return statements\n")

    checkParse(t, "return x;", true, true,
      {PROGRAMx,{RETURNxSTMT,{SIMPLExVAR,"x"}}},
      "return statement: variable")
    checkParse(t, "return x", false, true, nil,
      "bad return statement: no semicolon")
    checkParse(t, "return -34;", true, true,
      {PROGRAMx,{RETURNxSTMT,{{UNxOP,"-"},{NUMLITxVAL,"34"}}}},
      "return statement: number")
    checkParse(t, "return;", false, false, nil,
      "return statement: no argument")
    checkParse(t, "return(x);", true, true,
      {PROGRAMx,{RETURNxSTMT,{SIMPLExVAR,"x"}}},
      "return statement: variable in parentheses")
    checkParse(t, "return(3+true<=4*(x-y));", true, true,
      {PROGRAMx,{RETURNxSTMT,{{BINxOP,"<="},{{BINxOP,"+"},{NUMLITxVAL,
        "3"},{BOOLLITxVAL,"true"}},{{BINxOP,"*"},{NUMLITxVAL,"4"},
        {{BINxOP,"-"},{SIMPLExVAR,"x"},{SIMPLExVAR,"y"}}}}}},
      "return statement: fancier expression")
end


function test_output_stmt_with_expr(t)
    io.write("Test Suite: output statements - with expressions\n")

    checkParse(t, "output(x);", true, true,
      {PROGRAMx,{OUTPUTxSTMT,{SIMPLExVAR,"x"}}},
      "output statement: variable")
    checkParse(t, "output(char(65));", true, true,
      {PROGRAMx,{OUTPUTxSTMT,{CHARxCALL,{NUMLITxVAL,"65"}}}},
      "output statement: char call")
    checkParse(t, "output(char(1),char(2),char(3));", true, true,
      {PROGRAMx,{OUTPUTxSTMT,{CHARxCALL,{NUMLITxVAL,"1"}},{CHARxCALL,
        {NUMLITxVAL,"2"}},{CHARxCALL,{NUMLITxVAL,"3"}}}},
      "output statement: multiple char calls")
    checkParse(t, "output(\"a b\", char(1+2), a*4);", true, true,
      {PROGRAMx,{OUTPUTxSTMT,{STRLITxOUT,"\"a b\""},{CHARxCALL,
        {{BINxOP,"+"},{NUMLITxVAL,"1"},{NUMLITxVAL,"2"}}},{{BINxOP,"*"},
        {SIMPLExVAR,"a"},{NUMLITxVAL,"4"}}}},
      "output statement: string literal, char call, expression #1")
    checkParse(t, "output(char(1-2), \"a b\", 4/a);", true, true,
      {PROGRAMx,{OUTPUTxSTMT,{CHARxCALL,{{BINxOP,"-"},{NUMLITxVAL,"1"},
        {NUMLITxVAL,"2"}}},{STRLITxOUT,"\"a b\""},{{BINxOP,"/"},
        {NUMLITxVAL,"4"},{SIMPLExVAR,"a"}}}},
      "output statement: string literal, char call, expression #2")
    checkParse(t, "output(a+xyz_3[b*(c==d-f)]%g<=h);", true, true,
      {PROGRAMx,{OUTPUTxSTMT,{{BINxOP,"<="},{{BINxOP,"+"},{SIMPLExVAR,
        "a"},{{BINxOP,"%"},{ARRAYxVAR,"xyz_3",{{BINxOP,"*"},{SIMPLExVAR,
        "b"},{{BINxOP,"=="},{SIMPLExVAR,"c"},{{BINxOP,"-"},{SIMPLExVAR,
        "d"},{SIMPLExVAR,"f"}}}}},{SIMPLExVAR,"g"}}},{SIMPLExVAR,
        "h"}}}},
      "output statement: complex expression")
    checkParse(t, "output(1)", false, true, nil,
      "bad output statement: no semicolon")
end


function test_func_def_with_expr(t)
    io.write("Test Suite: function definitions - with expressions\n")

    checkParse(t, "def q(){output(abc+3);}", true, true,
      {PROGRAMx,{FUNCxDEF,"q",{PROGRAMx,{OUTPUTxSTMT,{{BINxOP,"+"},
        {SIMPLExVAR,"abc"},{NUMLITxVAL,"3"}}}}}},
      "function definition: with output expr")
    checkParse(t, "def qq(){output(a+x[b*(c==d-f)]%g<=h);}",
      true, true,
      {PROGRAMx,{FUNCxDEF,"qq",{PROGRAMx,{OUTPUTxSTMT,{{BINxOP,"<="},
        {{BINxOP,"+"},{SIMPLExVAR,"a"},{{BINxOP,"%"},{ARRAYxVAR,"x",
        {{BINxOP,"*"},{SIMPLExVAR,"b"},{{BINxOP,"=="},{SIMPLExVAR,"c"},
        {{BINxOP,"-"},{SIMPLExVAR,"d"},{SIMPLExVAR,"f"}}}}},{SIMPLExVAR,
        "g"}}},{SIMPLExVAR,"h"}}}}}},
      "function definition: complex expression")
end


function test_expr_prec_assoc(t)
    io.write("Test Suite: expressions - precedence & associativity\n")

    checkParse(t, "x=1and 2and 3and 4and 5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"and"},{{BINxOP,
        "and"},{{BINxOP, "and"},{{BINxOP,"and"},{NUMLITxVAL,"1"},
        {NUMLITxVAL,"2"}},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}},
        {NUMLITxVAL,"5"}}}},
      "Operator 'and' is left-associative")
    checkParse(t, "x=1 or 2 or 3 or 4 or 5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"or"},{{BINxOP,
        "or"},{{BINxOP, "or"},{{BINxOP,"or"},{NUMLITxVAL,"1"},
        {NUMLITxVAL,"2"}},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}},
        {NUMLITxVAL,"5"}}}},
      "Operator 'or' is left-associative")
    checkParse(t, "x=1+2+3+4+5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},{{BINxOP,
        "+"},{{BINxOP, "+"},{{BINxOP,"+"},{NUMLITxVAL,"1"},{NUMLITxVAL,
        "2"}},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}},{NUMLITxVAL,"5"}}}},
      "Binary operator + is left-associative")
    checkParse(t, "x=1-2-3-4-5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"-"},{{BINxOP,
        "-"},{{BINxOP, "-"},{{BINxOP,"-"},{NUMLITxVAL,"1"},{NUMLITxVAL,
        "2"}},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}},{NUMLITxVAL,"5"}}}},
      "Binary operator - is left-associative")
    checkParse(t, "x=1*2*3*4*5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"*"},{{BINxOP,
        "*"},{{BINxOP, "*"},{{BINxOP,"*"},{NUMLITxVAL,"1"},{NUMLITxVAL,
        "2"}},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}},{NUMLITxVAL,"5"}}}},
      "Operator * is left-associative")
    checkParse(t, "x=1/2/3/4/5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"/"},{{BINxOP,
        "/"},{{BINxOP, "/"},{{BINxOP,"/"},{NUMLITxVAL,"1"},{NUMLITxVAL,
        "2"}},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}},{NUMLITxVAL,"5"}}}},
      "Operator / is left-associative")
    checkParse(t, "x=1%2%3%4%5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"%"},{{BINxOP,
        "%"},{{BINxOP, "%"},{{BINxOP,"%"},{NUMLITxVAL,"1"},{NUMLITxVAL,
        "2"}},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}},{NUMLITxVAL,"5"}}}},
      "Operator % is left-associative")
    checkParse(t, "x=1==2==3==4==5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},{{BINxOP,
        "=="},{{BINxOP, "=="},{{BINxOP,"=="},{NUMLITxVAL,"1"},
        {NUMLITxVAL,"2"}},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}},
        {NUMLITxVAL,"5"}}}},
      "Operator == is left-associative")
    checkParse(t, "x=1!=2!=3!=4!=5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"!="},{{BINxOP,
        "!="},{{BINxOP, "!="},{{BINxOP,"!="},{NUMLITxVAL,"1"},
        {NUMLITxVAL,"2"}},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}},
        {NUMLITxVAL,"5"}}}},
      "Operator != is left-associative")
    checkParse(t, "x=1<2<3<4<5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"<"},{{BINxOP,
        "<"},{{BINxOP, "<"},{{BINxOP,"<"},{NUMLITxVAL,"1"},{NUMLITxVAL,
        "2"}},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}},{NUMLITxVAL,"5"}}}},
      "Operator < is left-associative")
    checkParse(t, "x=1<=2<=3<=4<=5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"<="},{{BINxOP,
        "<="},{{BINxOP, "<="},{{BINxOP,"<="},{NUMLITxVAL,"1"},
        {NUMLITxVAL,"2"}},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}},
        {NUMLITxVAL,"5"}}}},
      "Operator <= is left-associative")
    checkParse(t, "x=1>2>3>4>5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">"},{{BINxOP,
        ">"},{{BINxOP, ">"},{{BINxOP,">"},{NUMLITxVAL,"1"},{NUMLITxVAL,
        "2"}},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}},{NUMLITxVAL,"5"}}}},
      "Operator > is left-associative")
    checkParse(t, "x=1>=2>=3>=4>=5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">="},{{BINxOP,
        ">="},{{BINxOP, ">="},{{BINxOP,">="},{NUMLITxVAL,"1"},
        {NUMLITxVAL,"2"}},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}},
        {NUMLITxVAL,"5"}}}},
      "Operator >= is left-associative")

    checkParse(t, "x=not not not not a;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{UNxOP,"not"},
        {{UNxOP,"not"},{{UNxOP,"not"},{{UNxOP,"not"},
        {SIMPLExVAR,"a"}}}}}}},
      "Operator 'not' is right-associative")
    checkParse(t, "x=++++a;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{UNxOP,"+"},{{UNxOP,"+"},
        {{UNxOP,"+"},{{UNxOP,"+"},{SIMPLExVAR,"a"}}}}}}},
      "Unary operator + is right-associative")
    checkParse(t, "x=- - - -a;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{UNxOP,"-"},{{UNxOP,"-"},
        {{UNxOP,"-"},{{UNxOP,"-"},{SIMPLExVAR,"a"}}}}}}},
      "Unary operator - is right-associative")

    checkParse(t, "x=a and b or c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"or"},{{BINxOP,
        "and"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: and, or")
    checkParse(t, "x=a and b==c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"and"},
        {SIMPLExVAR,"a"},{{BINxOP,"=="},{SIMPLExVAR,"b"},
        {SIMPLExVAR,"c"}}}}},
      "Precedence check: and, ==")
    checkParse(t, "x=a and b!=c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"and"},
        {SIMPLExVAR,"a"},{{BINxOP,"!="},{SIMPLExVAR,"b"},
        {SIMPLExVAR,"c"}}}}},
      "Precedence check: and, !=")
    checkParse(t, "x=a and b<c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"and"},
        {SIMPLExVAR,"a"},{{BINxOP,"<"},{SIMPLExVAR,"b"},
        {SIMPLExVAR,"c"}}}}},
      "Precedence check: and, <")
    checkParse(t, "x=a and b<=c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"and"},
        {SIMPLExVAR,"a"},{{BINxOP,"<="},{SIMPLExVAR,"b"},
        {SIMPLExVAR,"c"}}}}},
      "Precedence check: and, <=")
    checkParse(t, "x=a and b>c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"and"},
      {SIMPLExVAR,"a"},{{BINxOP,">"},{SIMPLExVAR,"b"},
      {SIMPLExVAR,"c"}}}}},
      "Precedence check: and, >")
    checkParse(t, "x=a and b>=c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"and"},
        {SIMPLExVAR,"a"},{{BINxOP,">="},{SIMPLExVAR,"b"},
        {SIMPLExVAR,"c"}}}}},
      "Precedence check: and, >=")
    checkParse(t, "x=a and b+c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"and"},
        {SIMPLExVAR,"a"},{{BINxOP,"+"},{SIMPLExVAR,"b"},
        {SIMPLExVAR,"c"}}}}},
      "Precedence check: and, binary +")
    checkParse(t, "x=a and b-c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"and"},
        {SIMPLExVAR,"a"},{{BINxOP,"-"},{SIMPLExVAR,"b"},
        {SIMPLExVAR,"c"}}}}},
      "Precedence check: and, binary -")
    checkParse(t, "x=a and b*c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"and"},
        {SIMPLExVAR,"a"},{{BINxOP,"*"},{SIMPLExVAR,"b"},
        {SIMPLExVAR,"c"}}}}},
      "Precedence check: and, *")
    checkParse(t, "x=a and b/c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"and"},
        {SIMPLExVAR,"a"},{{BINxOP,"/"},{SIMPLExVAR,"b"},
        {SIMPLExVAR,"c"}}}}},
      "Precedence check: and, /")
    checkParse(t, "x=a and b%c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"and"},
        {SIMPLExVAR,"a"},{{BINxOP,"%"},{SIMPLExVAR,"b"},
        {SIMPLExVAR,"c"}}}}},
      "Precedence check: and, %")

    checkParse(t, "x=a or b and c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"and"},{{BINxOP,
        "or"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: or, and")
    checkParse(t, "x=a or b==c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"or"},{SIMPLExVAR,
        "a"},{{BINxOP,"=="},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: or, ==")
    checkParse(t, "x=a or b!=c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"or"},{SIMPLExVAR,
        "a"},{{BINxOP,"!="},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check:  or , !=")
    checkParse(t, "x=a or b<c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"or"},{SIMPLExVAR,
        "a"},{{BINxOP,"<"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: or, <")
    checkParse(t, "x=a or b<=c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"or"},{SIMPLExVAR,
        "a"},{{BINxOP,"<="},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: or, <=")
    checkParse(t, "x=a or b>c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"or"},{SIMPLExVAR,
        "a"},{{BINxOP,">"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: or, >")
    checkParse(t, "x=a or b>=c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"or"},{SIMPLExVAR,
        "a"},{{BINxOP,">="},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: or, >=")
    checkParse(t, "x=a or b+c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"or"},{SIMPLExVAR,
        "a"},{{BINxOP,"+"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: or, binary +")
    checkParse(t, "x=a or b-c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"or"},{SIMPLExVAR,
        "a"},{{BINxOP,"-"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: or, binary -")
    checkParse(t, "x=a or b*c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"or"},{SIMPLExVAR,
        "a"},{{BINxOP,"*"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: or, *")
    checkParse(t, "x=a or b/c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"or"},{SIMPLExVAR,
        "a"},{{BINxOP,"/"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: or, /")
    checkParse(t, "x=a or b%c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"or"},{SIMPLExVAR,
        "a"},{{BINxOP,"%"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: or, %")

    checkParse(t, "x=a==b>c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">"},{{BINxOP,
        "=="},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: ==, >")
    checkParse(t, "x=a==b+c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},{SIMPLExVAR,
        "a"},{{BINxOP,"+"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: ==, binary +")
    checkParse(t, "x=a==b-c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},{SIMPLExVAR,
        "a"},{{BINxOP,"-"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: ==, binary -")
    checkParse(t, "x=a==b*c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},{SIMPLExVAR,
        "a"},{{BINxOP,"*"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: ==, *")
    checkParse(t, "x=a==b/c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},{SIMPLExVAR,
        "a"},{{BINxOP,"/"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: ==, /")
    checkParse(t, "x=a==b%c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},{SIMPLExVAR,
        "a"},{{BINxOP,"%"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: ==, %")

    checkParse(t, "x=a>b==c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},{{BINxOP,
        ">"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: >, ==")
    checkParse(t, "x=a>b+c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">"},{SIMPLExVAR,
        "a"},{{BINxOP,"+"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: >, binary +")
    checkParse(t, "x=a>b-c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">"},{SIMPLExVAR,
        "a"},{{BINxOP,"-"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: >, binary -")
    checkParse(t, "x=a>b*c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">"},{SIMPLExVAR,
        "a"},{{BINxOP,"*"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: >, *")
    checkParse(t, "x=a>b/c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">"},{SIMPLExVAR,
        "a"},{{BINxOP,"/"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: >, /")
    checkParse(t, "x=a>b%c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">"},{SIMPLExVAR,
        "a"},{{BINxOP,"%"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: >, %")

    checkParse(t, "x=a+b==c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},{{BINxOP,
        "+"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: binary +, ==")
    checkParse(t, "x=a+b>c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">"},{{BINxOP,
        "+"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: binary +, >")
    checkParse(t, "x=a+b-c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"-"},{{BINxOP,
        "+"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: binary +, binary -")
    checkParse(t, "x=a+b*c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},{SIMPLExVAR,
        "a"},{{BINxOP,"*"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: binary +, *")
    checkParse(t, "x=a+b/c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},{SIMPLExVAR,
        "a"},{{BINxOP,"/"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: binary +, /")
    checkParse(t, "x=a+b%c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},{SIMPLExVAR,
        "a"},{{BINxOP,"%"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: binary +, %")

    checkParse(t, "x=a-b==c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},{{BINxOP,
        "-"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: binary -, ==")
    checkParse(t, "x=a-b>c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">"},{{BINxOP,
        "-"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: binary -, >")
    checkParse(t, "x=a-b+c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},{{BINxOP,
        "-"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: binary -, binary +")
    checkParse(t, "x=a-b*c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"-"},{SIMPLExVAR,
        "a"},{{BINxOP,"*"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: binary -, *")
    checkParse(t, "x=a-b/c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"-"},{SIMPLExVAR,
        "a"},{{BINxOP,"/"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: binary -, /")
    checkParse(t, "x=a-b%c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"-"},{SIMPLExVAR,
        "a"},{{BINxOP,"%"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence check: binary -, %")

    checkParse(t, "x=a*b==c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},{{BINxOP,
        "*"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: *, ==")
    checkParse(t, "x=a*b>c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">"},{{BINxOP,
        "*"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: *, >")
    checkParse(t, "x=a*b+c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},{{BINxOP,
        "*"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: *, binary +")
    checkParse(t, "x=a*b-c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"-"},{{BINxOP,
        "*"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: *, binary -")
    checkParse(t, "x=a*b/c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"/"},{{BINxOP,
        "*"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: *, /")
    checkParse(t, "x=a*b%c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"%"},{{BINxOP,
        "*"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: *, %")

    checkParse(t, "x=a/b==c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},{{BINxOP,
        "/"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: /, ==")
    checkParse(t, "x=a/b>c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">"},{{BINxOP,
        "/"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: /, >")
    checkParse(t, "x=a/b+c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},{{BINxOP,
        "/"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: /, binary +")
    checkParse(t, "x=a/b-c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"-"},{{BINxOP,
        "/"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: /, binary -")
    checkParse(t, "x=a/b*c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"*"},{{BINxOP,
        "/"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: /, *")
    checkParse(t, "x=a/b%c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"%"},{{BINxOP,
        "/"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: /, %")

    checkParse(t, "x=a%b==c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},{{BINxOP,
        "%"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: %, ==")
    checkParse(t, "x=a%b>c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">"},{{BINxOP,
        "%"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: %, >")
    checkParse(t, "x=a%b+c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},{{BINxOP,
        "%"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: %, binary +")
    checkParse(t, "x=a%b-c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"-"},{{BINxOP,
        "%"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: %, binary -")
    checkParse(t, "x=a%b*c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"*"},{{BINxOP,
        "%"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: %, *")
    checkParse(t, "x=a%b/c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"/"},{{BINxOP,
        "%"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: %, /")

    checkParse(t, "x=not a and b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"and"},{{UNxOP,
        "not"},{SIMPLExVAR,"a"}},{SIMPLExVAR,"b"}}}},
      "Precedence check: not, and")
    checkParse(t, "x=not a or b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"or"},{{UNxOP,
        "not"},{SIMPLExVAR,"a"}},{SIMPLExVAR,"b"}}}},
      "Precedence check: not, or")
    checkParse(t, "x=not a==b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},{{UNxOP,
        "not"},{SIMPLExVAR,"a"}},{SIMPLExVAR,"b"}}}},
      "Precedence check: not, ==")
    checkParse(t, "x=not a!=b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"!="},{{UNxOP,
        "not"},{SIMPLExVAR,"a"}},{SIMPLExVAR,"b"}}}},
      "Precedence check: not, !=")
    checkParse(t, "x=not a<b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"<"},{{UNxOP,
        "not"},{SIMPLExVAR,"a"}},{SIMPLExVAR,"b"}}}},
      "Precedence check: not, <")
    checkParse(t, "x=not a<=b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"<="},{{UNxOP,
        "not"},{SIMPLExVAR,"a"}},{SIMPLExVAR,"b"}}}},
      "Precedence check: not, <=")
    checkParse(t, "x=not a>b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">"},{{UNxOP,
        "not"},{SIMPLExVAR,"a"}},{SIMPLExVAR,"b"}}}},
      "Precedence check: not, >")
    checkParse(t, "x=not a>=b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">="},{{UNxOP,
        "not"},{SIMPLExVAR,"a"}},{SIMPLExVAR,"b"}}}},
      "Precedence check: not, >=")
    checkParse(t, "x=not a+b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},{{UNxOP,
        "not"},{SIMPLExVAR,"a"}},{SIMPLExVAR,"b"}}}},
      "Precedence check: not, binary +")
    checkParse(t, "x=not a-b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"-"},{{UNxOP,
        "not"},{SIMPLExVAR,"a"}},{SIMPLExVAR,"b"}}}},
      "Precedence check: not, binary -")
    checkParse(t, "x=not a*b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"*"},{{UNxOP,
        "not"},{SIMPLExVAR,"a"}},{SIMPLExVAR,"b"}}}},
      "Precedence check: not, *")
    checkParse(t, "x=not a/b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"/"},{{UNxOP,
        "not"},{SIMPLExVAR,"a"}},{SIMPLExVAR,"b"}}}},
      "Precedence check: not, /")
    checkParse(t, "x=not a%b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"%"},{{UNxOP,
        "not"},{SIMPLExVAR,"a"}},{SIMPLExVAR,"b"}}}},
      "Precedence check: not, %")
    checkParse(t, "x=a!=+b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"!="},{SIMPLExVAR,
        "a"},{{UNxOP,"+"},{SIMPLExVAR,"b"}}}}},
      "Precedence check: !=, unary +")
    checkParse(t, "x=-a<c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"<"},{{UNxOP,
        "-"},{SIMPLExVAR,"a"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: unary -, <")
    checkParse(t, "x=a++b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},{SIMPLExVAR,
        "a"},{{UNxOP,"+"},{SIMPLExVAR,"b"}}}}},
      "Precedence check: binary +, unary +")
    checkParse(t, "x=a+-b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},{SIMPLExVAR,
        "a"},{{UNxOP,"-"},{SIMPLExVAR,"b"}}}}},
      "Precedence check: binary +, unary -")
    checkParse(t, "x=+a+b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},{{UNxOP,"+"},
        {SIMPLExVAR,"a"}},{SIMPLExVAR,"b"}}}},
      "Precedence check: unary +, binary +, *")
    checkParse(t, "x=-a+b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},{{UNxOP,"-"},
        {SIMPLExVAR,"a"}},{SIMPLExVAR,"b"}}}},
      "Precedence check: unary -, binary +")
    checkParse(t, "x=a-+b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"-"},{SIMPLExVAR,
        "a"},{{UNxOP,"+"},{SIMPLExVAR,"b"}}}}},
      "Precedence check: binary -, unary +")
    checkParse(t, "x=a- -b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"-"},{SIMPLExVAR,
        "a"},{{UNxOP,"-"},{SIMPLExVAR,"b"}}}}},
      "Precedence check: binary -, unary -")
    checkParse(t, "x=+a-b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"-"},{{UNxOP,"+"},
        {SIMPLExVAR,"a"}},{SIMPLExVAR,"b"}}}},
      "Precedence check: unary +, binary -, *")
    checkParse(t, "x=-a-b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"-"},{{UNxOP,"-"},
        {SIMPLExVAR,"a"}},{SIMPLExVAR,"b"}}}},
      "Precedence check: unary -, binary -")
    checkParse(t, "x=a*-b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"*"},{SIMPLExVAR,
        "a"},{{UNxOP,"-"},{SIMPLExVAR,"b"}}}}},
      "Precedence check: *, unary -")
    checkParse(t, "x=+a*c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"*"},{{UNxOP,"+"},
        {SIMPLExVAR,"a"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: unary +, *")
    checkParse(t, "x=a/+b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"/"},{SIMPLExVAR,
        "a"},{{UNxOP,"+"},{SIMPLExVAR,"b"}}}}},
      "Precedence check: /, unary +")
    checkParse(t, "x=-a/c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"/"},{{UNxOP,"-"},
        {SIMPLExVAR,"a"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: unary -, /")
    checkParse(t, "x=a%-b;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"%"},{SIMPLExVAR,
        "a"},{{UNxOP,"-"},{SIMPLExVAR,"b"}}}}},
      "Precedence check: %, unary -")
    checkParse(t, "x=+a%c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"%"},{{UNxOP,"+"},
        {SIMPLExVAR,"a"}},{SIMPLExVAR,"c"}}}},
      "Precedence check: unary +, %")

    checkParse(t, "x=1 and (2 and 3 and 4) and 5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"and"},{{BINxOP,
        "and"},{NUMLITxVAL,"1"},{{BINxOP,"and"},{{BINxOP,"and"},
          {NUMLITxVAL,"2"},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}}},
          {NUMLITxVAL,"5"}}}},
      "Associativity override: and")
    checkParse(t, "x=1 or (2 or 3 or 4) or 5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"or"},{{BINxOP,
        "or"},{NUMLITxVAL,"1"},{{BINxOP,"or"},{{BINxOP,"or"},
        {NUMLITxVAL,"2"},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}}},
        {NUMLITxVAL,"5"}}}},
      "Associativity override: or")
    checkParse(t, "x=1==(2==3==4)==5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},{{BINxOP,
        "=="},{NUMLITxVAL,"1"},{{BINxOP,"=="},{{BINxOP,"=="},
        {NUMLITxVAL,"2"},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}}},
        {NUMLITxVAL,"5"}}}},
      "Associativity override: ==")
    checkParse(t, "x=1!=(2!=3!=4)!=5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"!="},{{BINxOP,
        "!="},{NUMLITxVAL,"1"},{{BINxOP,"!="},{{BINxOP,"!="},
        {NUMLITxVAL,"2"},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}}},
        {NUMLITxVAL,"5"}}}},
      "Associativity override: !=")
    checkParse(t, "x=1<(2<3<4)<5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"<"},{{BINxOP,
        "<"},{NUMLITxVAL,"1"},{{BINxOP,"<"},{{BINxOP,"<"},{NUMLITxVAL,
        "2"},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}}},{NUMLITxVAL,"5"}}}},
      "Associativity override: <")
    checkParse(t, "x=1<=(2<=3<=4)<=5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"<="},{{BINxOP,
        "<="},{NUMLITxVAL,"1"},{{BINxOP,"<="},{{BINxOP,"<="},
        {NUMLITxVAL,"2"},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}}},
        {NUMLITxVAL,"5"}}}},
      "Associativity override: <=")
    checkParse(t, "x=1>(2>3>4)>5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">"},{{BINxOP,
        ">"},{NUMLITxVAL,"1"},{{BINxOP,">"},{{BINxOP,">"},{NUMLITxVAL,
        "2"},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}}},{NUMLITxVAL,"5"}}}},
      "Associativity override: >")
    checkParse(t, "x=1>=(2>=3>=4)>=5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">="},{{BINxOP,
        ">="},{NUMLITxVAL,"1"},{{BINxOP,">="},{{BINxOP,">="},
        {NUMLITxVAL,"2"},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}}},
        {NUMLITxVAL,"5"}}}},
      "Associativity override: >=")
    checkParse(t, "x=1+(2+3+4)+5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},
        {{BINxOP,"+"},{NUMLITxVAL,"1"},{{BINxOP,"+"},{{BINxOP,"+"},
        {NUMLITxVAL,"2"},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}}},
        {NUMLITxVAL,"5"}}}},
      "Associativity override: binary +")
    checkParse(t, "x=1-(2-3-4)-5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"-"},{{BINxOP,
        "-"},{NUMLITxVAL,"1"},{{BINxOP,"-"},{{BINxOP,"-"},{NUMLITxVAL,
        "2"},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}}},{NUMLITxVAL,"5"}}}},
      "Associativity override: binary -")
    checkParse(t, "x=1*(2*3*4)*5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"*"},{{BINxOP,
        "*"},{NUMLITxVAL,"1"},{{BINxOP,"*"},{{BINxOP,"*"},{NUMLITxVAL,
        "2"},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}}},{NUMLITxVAL,"5"}}}},
      "Associativity override: *")
    checkParse(t, "x=1/(2/3/4)/5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"/"},{{BINxOP,
        "/"},{NUMLITxVAL,"1"},{{BINxOP,"/"},{{BINxOP,"/"},{NUMLITxVAL,
        "2"},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}}},{NUMLITxVAL,"5"}}}},
      "Associativity override: /")
    checkParse(t, "x=1%(2%3%4)%5;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"%"},{{BINxOP,
        "%"},{NUMLITxVAL,"1"},{{BINxOP,"%"},{{BINxOP,"%"},{NUMLITxVAL,
        "2"},{NUMLITxVAL,"3"}},{NUMLITxVAL,"4"}}},{NUMLITxVAL,"5"}}}},
      "Associativity override: %")

    checkParse(t, "x=(a==b)+c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},{{BINxOP,
        "=="},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence override: ==, binary +")
    checkParse(t, "x=(a!=b)-c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"-"},{{BINxOP,
        "!="},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence override: !=, binary -")
    checkParse(t, "x=(a<b)*c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"*"},{{BINxOP,
        "<"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence override: <, *")
    checkParse(t, "x=(a<=b)/c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"/"},{{BINxOP,
        "<="},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence override: <=, /")
    checkParse(t, "x=(a>b)%c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"%"},{{BINxOP,
        ">"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence override: >, %")
    checkParse(t, "x=a+(b>=c);", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"+"},{SIMPLExVAR,
       "a"},{{BINxOP,">="},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence override: binary +, >=")
    checkParse(t, "x=(a-b)*c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"*"},{{BINxOP,
        "-"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence override: binary -, *")
    checkParse(t, "x=(a+b)%c;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"%"},{{BINxOP,
        "+"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},{SIMPLExVAR,"c"}}}},
      "Precedence override: binary +, %")
    checkParse(t, "x=a*(b==c);", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"*"},{SIMPLExVAR,
        "a"},{{BINxOP,"=="},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence override: *, ==")
    checkParse(t, "x=a/(b!=c);", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"/"},{SIMPLExVAR,
        "a"},{{BINxOP,"!="},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence override: /, !=")
    checkParse(t, "x=a%(b<c);", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"%"},{SIMPLExVAR,
        "a"},{{BINxOP,"<"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}}}}},
      "Precedence override: %, <")

    checkParse(t, "x=+(a<=b);", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{UNxOP,"+"},{{BINxOP,
        "<="},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}}}}},
      "Precedence override: unary +, <=")
    checkParse(t, "x=-(a>b);", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{UNxOP,"-"},{{BINxOP,">"},
        {SIMPLExVAR,"a"},{SIMPLExVAR,"b"}}}}},
      "Precedence override: unary -, >")
    checkParse(t, "x=+(a+b);", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{UNxOP,"+"},{{BINxOP,"+"},
        {SIMPLExVAR,"a"},{SIMPLExVAR,"b"}}}}},
      "Precedence override: unary +, binary +")
    checkParse(t, "x=-(a-b);", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{UNxOP,"-"},{{BINxOP,"-"},
        {SIMPLExVAR,"a"},{SIMPLExVAR,"b"}}}}},
      "Precedence override: unary -, binary -")
    checkParse(t, "x=+(a*b);", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{UNxOP,"+"},{{BINxOP,"*"},
        {SIMPLExVAR,"a"},{SIMPLExVAR,"b"}}}}},
      "Precedence override: unary +, *")
    checkParse(t, "x=-(a/b);", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{UNxOP,"-"},{{BINxOP,"/"},
        {SIMPLExVAR,"a"},{SIMPLExVAR,"b"}}}}},
      "Precedence override: unary -, /")
    checkParse(t, "x=+(a%b);", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{UNxOP,"+"},{{BINxOP,"%"},
        {SIMPLExVAR,"a"},{SIMPLExVAR,"b"}}}}},
      "Precedence override: unary +, %")
end


function test_inputnum_rand(t)
    io.write("Test Suite: inputnum & rand\n")

    checkParse(t, "x=inputnum();", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{INPUTxCALL}}},
      "Assignment with inputnum")
    checkParse(t, "x=inputnum(y);", false, false, nil,
      "Assignment with inputnum - nonempty parens")
    checkParse(t, "x=inputnum;", false, false, nil,
      "Assignment with inputnum - no parens")
    checkParse(t, "x=inputnum);", false, false, nil,
      "Assignment with inputnum - no left paren")
    checkParse(t, "x=inputnum(;", false, false, nil,
      "Assignment with inputnum - no right paren")
    checkParse(t, "inputnum();", true, false, nil,
      "inputnum as statement")
    checkParse(t, "x=inputnum();y=inputnum();", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{INPUTxCALL}},{ASSNxSTMT,
        {SIMPLExVAR,"y"},{INPUTxCALL}}},
      "Multiple assignments with inputnum")

    checkParse(t, "x=rand(1);", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{RANDxCALL,
      {NUMLITxVAL,"1"}}}},
      "Assignment with rand")
    checkParse(t, "x=rand();", false, false, nil,
      "Assignment with rand - empty parens")
    checkParse(t, "x=rand;", false, false, nil,
      "Assignment with rand - no parens")
    checkParse(t, "x=rand 1);", false, false, nil,
      "Assignment with rand - no left paren")
    checkParse(t, "x=rand(1;", false, false, nil,
      "Assignment with rand - no right paren")
    checkParse(t, "rand(1);", true, false, nil,
      "inputnum as statement")
    checkParse(t, "x=rand(a);y=rand(false);", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{RANDxCALL,
        {SIMPLExVAR,"a"}}},{ASSNxSTMT,{SIMPLExVAR,"y"},{RANDxCALL,
        {BOOLLITxVAL,"false"}}}},
      "Multiple assignments with rand")
end


function test_array_item(t)
    io.write("Test Suite: array items\n")

    checkParse(t, "a[1] = 2;", true, true,
      {PROGRAMx,{ASSNxSTMT,{ARRAYxVAR,"a",{NUMLITxVAL,"1"}},
        {NUMLITxVAL,"2"}}},
      "Array item in LHS of assignment")
    checkParse(t, "a = b[2];", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"a"},{ARRAYxVAR,"b",{NUMLITxVAL,
        "2"}}}},
      "Array item in RHS of assignment")
    checkParse(t, "abc[5*2+a]=bcd[5<=true/4]/cde[not false>x];",
      true, true,
      {PROGRAMx,{ASSNxSTMT,{ARRAYxVAR,"abc",{{BINxOP,"+"},{{BINxOP,
        "*"},{NUMLITxVAL,"5"},{NUMLITxVAL,"2"}},{SIMPLExVAR,"a"}}},
        {{BINxOP,"/"},{ARRAYxVAR,"bcd",{{BINxOP,"<="},{NUMLITxVAL,"5"},
        {{BINxOP,"/"},{BOOLLITxVAL,"true"},{NUMLITxVAL,"4"}}}},
        {ARRAYxVAR,"cde",{{BINxOP,">"},{{UNxOP,"not"},{BOOLLITxVAL,
        "false"}},{SIMPLExVAR,"x"}}}}}},
      "Array items: fancier")
end


function test_expr_complex(t)
    io.write("Test Suite: complex expressions\n")

    checkParse(t, "x=((((((((((((((((((((((((((((((((((((((((a)))"
      ..")))))))))))))))))))))))))))))))))))));", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{SIMPLExVAR,"a"}}},
      "Complex expression: many parens")
    checkParse(t, "x=(((((((((((((((((((((((((((((((((((((((a))))"
      .."))))))))))))))))))))))))))))))))))));", false, false, nil,
      "Bad complex expression: many parens, mismatch #1")
    checkParse(t, "x=((((((((((((((((((((((((((((((((((((((((a)))"
      .."))))))))))))))))))))))))))))))))))));", false, false, nil,
      "Bad complex expression: many parens, mismatch #2")
    checkParse(t, "x=a==b+c[x-y[2]]*+d!=e-f/-g<h+i%+j;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"<"},
        {{BINxOP,"!="},{{BINxOP,"=="},{SIMPLExVAR,"a"},{{BINxOP,"+"},
        {SIMPLExVAR,"b"},{{BINxOP,"*"},{ARRAYxVAR,"c",{{BINxOP,"-"},
        {SIMPLExVAR,"x"},{ARRAYxVAR,"y",{NUMLITxVAL,"2"}}}},{{UNxOP,
        "+"},{SIMPLExVAR,"d"}}}}},{{BINxOP,"-"},{SIMPLExVAR,"e"},
        {{BINxOP,"/"},{SIMPLExVAR,"f"},{{UNxOP,"-"},{SIMPLExVAR,
        "g"}}}}},{{BINxOP,"+"},{SIMPLExVAR,"h"},{{BINxOP,"%"},
        {SIMPLExVAR,"i"},{{UNxOP,"+"},{SIMPLExVAR,"j"}}}}}}},
      "Complex expression: misc #1")
    checkParse(t, "x=a==b+(c*+(d!=e[2*z]-f/-g)<h+i)%+j;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,"=="},{SIMPLExVAR,
        "a"},{{BINxOP,"+"},{SIMPLExVAR,"b"},{{BINxOP,"%"},{{BINxOP,"<"},
        {{BINxOP,"*"},{SIMPLExVAR,"c"},{{UNxOP,"+"},{{BINxOP,"!="},
        {SIMPLExVAR,"d"},{{BINxOP,"-"},{ARRAYxVAR,"e",{{BINxOP,"*"},
        {NUMLITxVAL,"2"},{SIMPLExVAR,"z"}}},{{BINxOP,"/"},{SIMPLExVAR,
        "f"},{{UNxOP,"-"},{SIMPLExVAR,"g"}}}}}}},{{BINxOP,"+"},
        {SIMPLExVAR,"h"},{SIMPLExVAR,"i"}}},{{UNxOP,"+"},{SIMPLExVAR,
        "j"}}}}}}},
      "Complex expression: misc #2")
    checkParse(t, "x=a[x[y[z]]%4]++b*c<=d- -e/f>g+-h%i>=j;", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">="},{{BINxOP,
        ">"},{{BINxOP,"<="},{{BINxOP,"+"},{ARRAYxVAR,"a",{{BINxOP,"%"},
        {ARRAYxVAR,"x",{ARRAYxVAR,"y",{SIMPLExVAR,"z"}}},{NUMLITxVAL,
        "4"}}},{{BINxOP,"*"},{{UNxOP,"+"},{SIMPLExVAR,"b"}},{SIMPLExVAR,
        "c"}}},{{BINxOP,"-"},{SIMPLExVAR,"d"},{{BINxOP,"/"},
        {{UNxOP,"-"},{SIMPLExVAR,"e"}},{SIMPLExVAR,"f"}}}},
        {{BINxOP,"+"},{SIMPLExVAR,"g"},{{BINxOP,"%"},{{UNxOP,"-"},
        {SIMPLExVAR,"h"}},{SIMPLExVAR,"i"}}}},{SIMPLExVAR,"j"}}}},
      "Complex expression: misc #3")
    checkParse(t, "x=a++(b*c<=d)- -e/(f>g+-h%i)>=j[-z];", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{{BINxOP,">="},
        {{BINxOP,"-"},{{BINxOP,"+"},{SIMPLExVAR,"a"},{{UNxOP,"+"},
        {{BINxOP,"<="},{{BINxOP,"*"},{SIMPLExVAR,"b"},{SIMPLExVAR,"c"}},
        {SIMPLExVAR,"d"}}}},{{BINxOP,"/"},{{UNxOP,"-"},
        {SIMPLExVAR,"e"}},{{BINxOP,">"},{SIMPLExVAR,"f"},{{BINxOP,"+"},
        {SIMPLExVAR,"g"},{{BINxOP,"%"},{{UNxOP,"-"},{SIMPLExVAR,"h"}},
        {SIMPLExVAR,"i"}}}}}},{ARRAYxVAR,"j",{{UNxOP,"-"},
        {SIMPLExVAR,"z"}}}}}},
      "Complex expression: misc #4")
    checkParse(t,
    "output(rand(inputnum()==15e3),rand(rand(rand(inputnum()))));",
      true, true,
     {PROGRAMx,{OUTPUTxSTMT,{RANDxCALL,{{BINxOP,"=="},{INPUTxCALL},
       {NUMLITxVAL,"15e3"}}},{RANDxCALL,{RANDxCALL,{RANDxCALL,
       {INPUTxCALL}}}}}},
     "Complex expression with output")
    checkParse(t, "x=a==b+c*+d!=e-/-g<h+i%+j;",
      false, false, nil,
      "Bad complex expression: misc #1")
    checkParse(t, "x=a==b+(c*+(d!=e-f/-g)<h+i)%+;",
      false, false, nil,
      "Bad complex expression: misc #2")
    checkParse(t, "x=a++b*c<=d- -e x/f>g+-h%i>=j;",
      false, false, nil,
      "Bad complex expression: misc #3")
    checkParse(t, "x=a++b*c<=d)- -e/(f>g+-h%i)>=j;",
      false, false, nil,
      "Bad complex expression: misc #4")

    checkParse(t, "x=((a[(b[c[(d[((e[f]))])]])]));", true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{ARRAYxVAR,"a",
        {ARRAYxVAR,"b",{ARRAYxVAR,"c",{ARRAYxVAR,"d",{ARRAYxVAR,"e",
        {SIMPLExVAR,"f"}}}}}}}},
      "Complex expression: many parens/brackets")
    checkParse(t, "x=((a[(b[c[(d[((e[f]))]])])]));", false, false, nil,
      "Bad complex expression: mismatched parens/brackets")

    checkParse(t, "while((a+b)%d+a()!=true){output();}", true, true,
      {PROGRAMx,{WHILExLOOP,{{BINxOP,"!="},{{BINxOP,"+"},
        {{BINxOP,"%"},{{BINxOP,"+"},{SIMPLExVAR,"a"},{SIMPLExVAR,"b"}},
        {SIMPLExVAR,"d"}},{FUNCxCALL,"a"}},{BOOLLITxVAL,"true"}},
        {PROGRAMx,{OUTPUTxSTMT}}}},
      "While loop with complex expression")
    checkParse(t, "if(6e+5==true/((q()))+-+-+-false){a=3;}elseif"
      .."(3+4+5){x=5;}else{r=7;}", true, true,
      {PROGRAMx,{IFxSTMT,{{BINxOP,"=="},{NUMLITxVAL,"6e+5"},{{BINxOP,
        "+"},{{BINxOP,"/"},{BOOLLITxVAL,"true"},{FUNCxCALL,"q"}},
        {{UNxOP,"-"},{{UNxOP,"+"},{{UNxOP,"-"},{{UNxOP,"+"},{{UNxOP,
        "-"},{BOOLLITxVAL,"false"}}}}}}}},{PROGRAMx,{ASSNxSTMT,
        {SIMPLExVAR,"a"},{NUMLITxVAL,"3"}}},{{BINxOP,"+"},{{BINxOP,"+"},
        {NUMLITxVAL,"3"},{NUMLITxVAL,"4"}},{NUMLITxVAL,"5"}},{PROGRAMx,
        {ASSNxSTMT,{SIMPLExVAR,"x"},{NUMLITxVAL,"5"}}},{PROGRAMx,
        {ASSNxSTMT,{SIMPLExVAR,"r"},{NUMLITxVAL,"7"}}}}},
      "If statement with complex expression")
end


function test_prog(t)
    io.write("Test Suite: complete programs\n")

    -- Example #1 from Assignment 4 description
    checkParse(t,
      [[#!
        # Nilgai Example #1
        # Glenn G. Chappell
        # 2024-02-14
        x = 3;  # Set a variable
        output(x+4, eol);
      ]], true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"x"},{NUMLITxVAL,"3"}},
        {OUTPUTxSTMT,{{BINxOP,"+"},{SIMPLExVAR,"x"},{NUMLITxVAL,"4"}},
        {EOLxOUT}}},
      "Program: Example #1 from Assignment 4 description")

    -- Fibonacci Example
    checkParse(t,
      [[#!
      # fibo.ngai
      # Glenn G. Chappell
      # 2024-02-14
      #
      # For CS 331 Spring 2024
      # Compute Fibonacci Numbers


      # The Fibonacci number F(n), for n >= 0, is defined by F(0) = 0,
      # F(1) = 1, and for n >= 2, F(n) = F(n-2) + F(n-1).


      # fibo (param in variable n)
      # Return Fibonacci number F(n).
      def fibo()
      {
          # Variables holding consecutive Fibonacci numbers
          currfib = 0;
          nextfib = 1;

          # Advance currfib, nextfib as many times as needed
          i = 0;  # Loop counter
          while(i < n)
          {
              tmp = currfib + nextfib;
              currfib = nextfib;
              nextfib = tmp;
              i = i+1;
          }
          return currfib;
      }


      # Main program
      # Print some Fibonacci numbers, nicely formatted.
      how_many_to_print = 20;

      output("Fibonacci Numbers", eol);

      j = 0;  # Loop counter
      while(j < how_many_to_print)
      {
          n = j;  # Set param for fibo
          ff = fibo();
          output("F(", j, ") = ", ff, eol);
          j = j+1;
      }
      ]], true, true,
      {PROGRAMx,{FUNCxDEF,"fibo",{PROGRAMx,{ASSNxSTMT,
      {SIMPLExVAR,"currfib"},{NUMLITxVAL,"0"}},{ASSNxSTMT,
      {SIMPLExVAR,"nextfib"},{NUMLITxVAL,"1"}},{ASSNxSTMT,
      {SIMPLExVAR,"i"},{NUMLITxVAL,"0"}},{WHILExLOOP,{{BINxOP,"<"},
      {SIMPLExVAR,"i"},{SIMPLExVAR,"n"}},{PROGRAMx,{ASSNxSTMT,
      {SIMPLExVAR,"tmp"},{{BINxOP,"+"},{SIMPLExVAR,"currfib"},
      {SIMPLExVAR,"nextfib"}}},{ASSNxSTMT,{SIMPLExVAR,"currfib"},
      {SIMPLExVAR,"nextfib"}},{ASSNxSTMT,{SIMPLExVAR,"nextfib"},
      {SIMPLExVAR,"tmp"}},{ASSNxSTMT,{SIMPLExVAR,"i"},{{BINxOP,"+"},
      {SIMPLExVAR,"i"},{NUMLITxVAL,"1"}}}}},{RETURNxSTMT,
      {SIMPLExVAR,"currfib"}}}},{ASSNxSTMT,
      {SIMPLExVAR,"how_many_to_print"},{NUMLITxVAL,"20"}},{OUTPUTxSTMT,
      {STRLITxOUT,"\"Fibonacci Numbers\""},{EOLxOUT}},{ASSNxSTMT,
      {SIMPLExVAR,"j"},{NUMLITxVAL,"0"}},{WHILExLOOP,{{BINxOP,"<"},
      {SIMPLExVAR,"j"},{SIMPLExVAR,"how_many_to_print"}},{PROGRAMx,
      {ASSNxSTMT,{SIMPLExVAR,"n"},{SIMPLExVAR,"j"}},{ASSNxSTMT,
      {SIMPLExVAR,"ff"},{FUNCxCALL,"fibo"}},{OUTPUTxSTMT,
      {STRLITxOUT,"\"F(\""},{SIMPLExVAR,"j"},{STRLITxOUT,"\") = \""},
      {SIMPLExVAR,"ff"},{EOLxOUT}},{ASSNxSTMT,{SIMPLExVAR,"j"},
      {{BINxOP,"+"},{SIMPLExVAR,"j"},{NUMLITxVAL,"1"}}}}}},
      "Program: Fibonacci Example")

    -- Input number, print its square
    checkParse(t,
      [[#!
        output("Type a number: ");
        a = inputnum();
        output(eol, eol);
        output("You typed: ");
        output(a, eol);
        output("Its square is: ");
        output(a*a, eol, eol);
      ]], true, true,
      {PROGRAMx,{OUTPUTxSTMT,{STRLITxOUT,"\"Type a number: \""}},
        {ASSNxSTMT,{SIMPLExVAR,"a"},{INPUTxCALL}},{OUTPUTxSTMT,
        {EOLxOUT},{EOLxOUT}},{OUTPUTxSTMT,
        {STRLITxOUT,"\"You typed: \""}},{OUTPUTxSTMT,{SIMPLExVAR,"a"},
        {EOLxOUT}},{OUTPUTxSTMT,
        {STRLITxOUT,"\"Its square is: \""}},{OUTPUTxSTMT,{{BINxOP,"*"},
        {SIMPLExVAR,"a"},{SIMPLExVAR,"a"}},{EOLxOUT},{EOLxOUT}}},
      "Program: Input number, print its square")

    -- Input numbers, stop at sentinel, print even/odd
    checkParse(t,
      [[#!
        continue = true;
        while (continue)
        {
            output("Type a number (0 to end): ");
            n = inputnum();
            output(eol, eol);
            if (n == 0)
            {
                continue = false;
            }
            else
            {
                output("The number ", n, " is ");
                if (n % 2 == 0)
                {
                    output("even");
                }
                else
                {
                    output("odd");
                }
                output(eol, eol);
            }
        }
        output("Bye!", eol, eol);
      ]], true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"continue"},
      {BOOLLITxVAL,"true"}},{WHILExLOOP,{SIMPLExVAR,"continue"},
      {PROGRAMx,{OUTPUTxSTMT,
      {STRLITxOUT,"\"Type a number (0 to end): \""}},{ASSNxSTMT,
      {SIMPLExVAR,"n"},{INPUTxCALL}},{OUTPUTxSTMT,{EOLxOUT},{EOLxOUT}},
      {IFxSTMT,{{BINxOP,"=="},{SIMPLExVAR,"n"},{NUMLITxVAL,"0"}},
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"continue"},
      {BOOLLITxVAL,"false"}}},{PROGRAMx,{OUTPUTxSTMT,
      {STRLITxOUT,"\"The number \""},{SIMPLExVAR,"n"},
      {STRLITxOUT,"\" is \""}},{IFxSTMT,{{BINxOP,"=="},{{BINxOP,"%"},
      {SIMPLExVAR,"n"},{NUMLITxVAL,"2"}},{NUMLITxVAL,"0"}},{PROGRAMx,
      {OUTPUTxSTMT,{STRLITxOUT,"\"even\""}}},{PROGRAMx,{OUTPUTxSTMT,
      {STRLITxOUT,"\"odd\""}}}},{OUTPUTxSTMT,{EOLxOUT},{EOLxOUT}}}}}},
      {OUTPUTxSTMT,{STRLITxOUT,"\"Bye!\""},{EOLxOUT},{EOLxOUT}}},
      "Program: Input numbers, stop at sentinel, print even/odd")

    -- Input numbers, print them in reverse order
    checkParse(t,
      [[#!
        howMany = 5;  # How many numbers to input
        output("I will ask you for ", howMany, " numbers.", eol);
        output("Then I will print them in reverse order.", eol, eol);
        i = 1;
        while (i <= howMany)  # Input loop
        {
            output("Type value #", i, ": ");
            v[i] = inputnum();
            output(eol, eol);
            i = i+1;
        }
        output("------------------------------------", eol, eol);
        output("Here are the values, in reverse order:", eol);
        i = howMany;
        while (i > 0)  # Output loop
        {
            output("Value #", i, ": ", v[i], eol);
            i = i-1;
        }
        output(eol);
      ]], true, true,
      {PROGRAMx,{ASSNxSTMT,{SIMPLExVAR,"howMany"},{NUMLITxVAL,"5"}},
      {OUTPUTxSTMT,{STRLITxOUT,"\"I will ask you for \""},
      {SIMPLExVAR,"howMany"},{STRLITxOUT,"\" numbers.\""},{EOLxOUT}},
      {OUTPUTxSTMT,
      {STRLITxOUT,"\"Then I will print them in reverse order.\""},
      {EOLxOUT},{EOLxOUT}},{ASSNxSTMT,{SIMPLExVAR,"i"},
      {NUMLITxVAL,"1"}},{WHILExLOOP,{{BINxOP,"<="},{SIMPLExVAR,"i"},
      {SIMPLExVAR,"howMany"}},{PROGRAMx,{OUTPUTxSTMT,
      {STRLITxOUT,"\"Type value #\""},{SIMPLExVAR,"i"},
      {STRLITxOUT,"\": \""}},{ASSNxSTMT,{ARRAYxVAR,"v",
      {SIMPLExVAR,"i"}},{INPUTxCALL}},{OUTPUTxSTMT,{EOLxOUT},{EOLxOUT}},
      {ASSNxSTMT,{SIMPLExVAR,"i"},{{BINxOP,"+"},{SIMPLExVAR,"i"},
      {NUMLITxVAL,"1"}}}}},{OUTPUTxSTMT,
      {STRLITxOUT,"\"------------------------------------\""},{EOLxOUT},
      {EOLxOUT}},{OUTPUTxSTMT,
      {STRLITxOUT,"\"Here are the values, in reverse order:\""},
      {EOLxOUT}},{ASSNxSTMT,{SIMPLExVAR,"i"},{SIMPLExVAR,"howMany"}},
      {WHILExLOOP,{{BINxOP,">"},{SIMPLExVAR,"i"},{NUMLITxVAL,"0"}},
      {PROGRAMx,{OUTPUTxSTMT,{STRLITxOUT,"\"Value #\""},
      {SIMPLExVAR,"i"},{STRLITxOUT,"\": \""},{ARRAYxVAR,"v",
      {SIMPLExVAR,"i"}},{EOLxOUT}},{ASSNxSTMT,{SIMPLExVAR,"i"},
      {{BINxOP,"-"},{SIMPLExVAR,"i"},{NUMLITxVAL,"1"}}}}},{OUTPUTxSTMT,
      {EOLxOUT}}},
      "Program: Input numbers, print them in reverse order")

    -- Long program
    howmany = 200
    progpiece = "output(42);"
    prog = progpiece:rep(howmany)
    ast = {PROGRAMx}
    astpiece = {OUTPUTxSTMT,{NUMLITxVAL,"42"}}
    for i = 1, howmany do
        table.insert(ast, astpiece)
    end
    checkParse(t, prog, true, true,
      ast,
      "Program: Long program")

    -- Very long program
    howmany = 30000
    progpiece = "x=inputnum();output(x,eol);"
    prog = progpiece:rep(howmany)
    ast = {PROGRAMx}
    astpiece1 = {ASSNxSTMT,{SIMPLExVAR,"x"},{INPUTxCALL}}
    astpiece2 = {OUTPUTxSTMT,{SIMPLExVAR,"x"},{EOLxOUT}}
    for i = 1, howmany do
        table.insert(ast, astpiece1)
        table.insert(ast, astpiece2)
    end
    checkParse(t, prog, true, true,
      ast,
      "Program: Very long program")
end


function test_parseit(t)
    io.write("TEST SUITES FOR MODULE parseit\n")
    test_simple(t)
    test_output_stmt_no_expr(t)
    test_function_call_stmt(t)
    test_func_def_no_expr(t)
    test_while_loop_simple_expr(t)
    test_if_stmt_simple_expr(t)
    test_assn_stmt(t)
    test_return_stmt(t)
    test_output_stmt_with_expr(t)
    test_func_def_with_expr(t)
    test_expr_prec_assoc(t)
    test_inputnum_rand(t)
    test_array_item(t)
    test_expr_complex(t)
    test_prog(t)
end


-- *********************************************************************
-- Main Program
-- *********************************************************************


test_parseit(tester)
io.write("\n")
endMessage(tester:allPassed())

-- Terminate program, signaling no error
terminate(0)

