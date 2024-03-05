-- use_parseit.lua
-- Glenn G. Chappell
-- 2024-02-20
--
-- For CS 331 Spring 2024
-- Simple Main Program for parseit Module
-- Requires parseit.lua

parseit = require "src.Assignment 4.parseit"


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
        if string.sub(x, 1, 1) == '"' then
            io.write("'"..x.."'")
        else
            io.write('"'..x..'"')
        end
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
        if not first then
            io.write(" ")
        end
        io.write("}")
    end
end


-- Separator string
dashes = ("-"):rep(72)  -- Lots of dashes


-- check
-- Given a "program", check its syntactic correctness using parseit.
-- Print results.
function check(program)
    io.write("Program: "..program.."\n")

    local good, done, ast = parseit.parse(program)
    assert(type(good) == "boolean")
    assert(type(done) == "boolean")
    if good then
        assert(type(ast) == "table")
    end

    if good and done then
        io.write("Good! - AST: ")
        printAST_parseit(ast)
        io.write("\n")
    elseif good and not done then
        io.write("BAD - extra characters at end\n")
    elseif not good and done then
        io.write("UNFINISHED - more is needed\n")
    else  -- not good and not done
        io.write("BAD - syntax error\n")
    end

    io.write(dashes.."\n")
end


-- Main program
-- Check several "programs".
io.write("Recursive-Descent Parser: Nilgai\n")
io.write(dashes.."\n")

io.write("### The following 6 programs will parse correcly with\n")
io.write("### parseit.lua as posted in the Git repository.\n")
io.write(dashes.."\n")

check("")
check("output();")
check("output(eol);")
check("output('abc','def',eol);output('xyz');output(eol);")
check("def f(){}")
check("def g(){def h(){} output(eol); output(eol);} output('y',eol);")

io.write("\n")
io.write(dashes.."\n")
io.write("### The following 6 programs should parse correcly.\n")
io.write("### However, parseit.lua from the Git repository may give")
io.write(" incorrect results.\n")
io.write(dashes.."\n")

check("a=3;")
check("a=a+1;")
check("a=inputnum();")
check("output(a+1);")
check("a=3; output(a+b, eol);")
check("a[e*2+1]=2;")

io.write("\n")
io.write(dashes.."\n")
io.write("### The program below should have the AST from")
io.write(" the Assignment 4 description.\n")
io.write(dashes.."\n")

check("# Nilgai Example #1\n# Glenn G. Chappell\n"..
      "# 2024-02-14\nx = 3;  # Set a variable\noutput(x+4, eol);\n")

io.write("\n")
io.write(dashes.."\n")
io.write("### The program below should get the result:")
io.write(" \"BAD - extra characters at end\"\n")
io.write(dashes.."\n")

check("output();elseif")

io.write("\n")
io.write(dashes.."\n")
io.write("### The program below should get the result:")
io.write(" \"UNFINISHED - more is needed\"\n")
io.write(dashes.."\n")

check("def foo() {output(eol")

io.write("\n")
io.write(dashes.."\n")
io.write("### The program below should get the result:")
io.write(" \"BAD - syntax error\"\n")
io.write(dashes.."\n")

check("if(a)b")

