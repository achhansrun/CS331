local lexit = {}


-- Numeric constants representing lexeme categories
lexit.KEY    = 1
lexit.ID     = 2
lexit.NUMLIT = 3
lexit.STRLIT = 4
lexit.OP     = 5
lexit.PUNCT  = 6
lexit.MAL    = 7


-- catnames
-- Array of names of lexeme categories.
-- Human-readable strings. Indices are above numeric constants.
lexit.catnames = {
    "Keyword",
    "Identifier",
    "NumericLiteral",
    "StringLiteral",
    "Operator",
    "Punctuation",
    "Malformed"
}

-- *********************************************************************
-- Kind-of-Character Functions
-- *********************************************************************

-- All functions return false when given a string whose length is not
-- exactly 1.


-- isLetter
-- Returns true if string c is a letter character, false otherwise.
local function isLetter(c)
    if c:len() ~= 1 then
        return false
    elseif c >= "A" and c <= "Z" then
        return true
    elseif c >= "a" and c <= "z" then
        return true
    else
        return false
    end
end


-- isDigit
-- Returns true if string c is a digit character, false otherwise.
local function isDigit(c)
    if c:len() ~= 1 then
        return false
    elseif c >= "0" and c <= "9" then
        return true
    else
        return false
    end
end


-- isWhitespace
-- Returns true if string c is a whitespace character, false otherwise.
local function isWhitespace(c)
    if c:len() ~= 1 then
        return false
    elseif c == " " or c == "\t" or c == "\n" or c == "\r"
      or c == "\f" then
        return true
    else
        return false
    end
end


-- isPrintableASCII
-- Returns true if string c is a printable ASCII character (codes 32 " "
-- through 126 "~"), false otherwise.
local function isPrintableASCII(c)
    if c:len() ~= 1 then
        return false
    elseif c >= " " and c <= "~" then
        return true
    else
        return false
    end
end


-- isIllegal
-- Returns true if string c is an illegal character, false otherwise.
local function isIllegal(c)
    if c:len() ~= 1 then
        return false
    elseif isWhitespace(c) then
        return false
    elseif isPrintableASCII(c) then
        return false
    else
        return true
    end
end

-- *********************************************************************
-- The Lexer
-- *********************************************************************

-- lexit
function lexit.lex(program)
-- ***** Variables (like class data members) *****

local pos       -- Index of next character in program
                -- INVARIANT: when getLexeme is called, pos is
                --  EITHER the index of the first character of the
                --  next lexeme OR program:len()+1
local state     -- Current state for our state machine
local ch        -- Current character
local lexstr    -- The lexeme, so far
local category  -- Category of lexeme, set when state set to DONE
local handlers  -- Dispatch table; value created later

-- ***** States *****

local DONE     = 0
local START    = 1
local LETTER   = 2
local DIGIT    = 3
local DIGDOT   = 4
local OP       = 5
local STRING   = 6
local EXPONENT = 7
local MAL      = 8
local PUNCT    = 9

-- ***** Character-Related Utility Functions *****

    -- currChar
    -- Return the current character, at index pos in program. Return
    -- value is a single-character string, or the empty string if pos is
    -- past the end.
    local function currChar()
    return program:sub(pos, pos)
    end
    -- nextChar
    -- Return the next character, at index pos+1 in program. Return
    -- value is a single-character string, or the empty string if pos+1
    -- is past the end.
    local function nextChar()
        return program:sub(pos+1, pos+1)
    end

    -- drop1
    -- Move pos to the next character.
    local function drop1()
        pos = pos+1
    end

    -- add1
    -- Add the current character to the lexeme, moving pos to the next
    -- character.
    local function add1()
        lexstr = lexstr .. currChar()
        drop1()
    end

    -- skipToNextLexeme
    -- Skip whitespace and comments, moving pos to the beginning of
    -- the next lexeme, or to program:len()+1.
    local function skipToNextLexeme()
        while true do
            -- Skip whitespace characters
            while isWhitespace(currChar()) do
                drop1()
            end

            -- Done if no comment
            if currChar() ~= "#" then
                break
            end

            -- Skip comment
            drop1()  -- Drop leading "#"
            while true do
                if currChar() == "\n" or currChar() == "" then
                    drop1()  -- Drop trailing "\n"
                    break
                end
                drop1()  -- Drop character inside comment
            end
        end
    end

    -- ***** State-Handler Functions *****

    -- A function with a name like handle_XYZ is the handler function
    -- for state XYZ

    -- State DONE: lexeme is done; this handler should not be called.
    local function handle_DONE()
        error("'DONE' state should not be handled\n")
    end

    -- State START: no character read yet.
    local function handle_START()
        if isLetter(ch) or ch == "_" then
            add1()
            state = LETTER --move to handle_LETTER
        elseif isDigit(ch) then
            add1()
            state = DIGIT --mvoe to handle_DIGIT
        elseif ch =='"' or ch == "'" then
            add1()
            state = STRING --move to handle_STRING
        elseif ch == "=" or ch == "<" or ch == ">" or ch == "+" or ch == "-" or ch == "*" or ch == "/" or ch == "%" or ch == "[" or ch == "]" then
            add1()
            state = OP --move to handle_OP
        elseif not isIllegal(ch) then
            add1()
            state = PUNCT --move to handle_PUNCT
        else
            add1()
            state = DONE --move to handle_DON
            category = lexit.MAL --tag as MAL
        end
    end

    -- State LETTER: we are in an ID.
    local function handle_LETTER()
        if isLetter(ch) or isDigit(ch) or ch == "_" then
            add1()
        else
            state = DONE --move to handle_DONE
            if lexstr == "and" or lexstr == "char"
              or lexstr == "def" or lexstr == "else" or lexstr == "def"
              or lexstr == "elseif" or lexstr == "eol" or lexstr == "false"
              or lexstr == "if" or lexstr == "inputnum" or lexstr == "not"
              or lexstr == "or" or lexstr == "output" or lexstr == "rand"
              or lexstr == "return" or lexstr == "true" or lexstr == "while"
              then
                category = lexit.KEY --tag as KEY
            else
                category = lexit.ID --tag as ID
            end
        end
    end

    -- State DIGIT: we are in a NUMLIT, and we have NOT seen ".".
    local function handle_DIGIT()
        if isDigit(ch) then
            add1()
        elseif ch == "." then
                state = DIGDOT --move to handle_DIGDOT
        elseif ch == "e" or ch == "E" then
            if nextChar() == "+" then
                if isDigit(program:sub(pos+2, pos+2)) then
                    add1()
                    add1()
                    state = EXPONENT --move to handle_EXPONENT
                else 
                    state = DONE --move to handle_DONE
                    category = lexit.NUMLIT --tag as NUMLIT
                end
            elseif isDigit(nextChar()) then
                add1()
                state = EXPONENT --move to handle_EXPONENT
            else
                state = DONE --move to handle_DONE
                category = lexit.NUMLIT --tag as NUMLIT
            end

-- **************************************************************
            -- This section was causing issues for the Parseit.lua file so has been commented out
        -- elseif ch == "+" then
        --     if isDigit(nextChar()) then
        --         add1()
        --     else
        --         state = DONE --move to handle_DONE
        --         category = lexit.NUMLIT --tag as NUMLIT
        --     end

-- *************************************************************

        else
            state = DONE --move to handle_DONE
            category = lexit.NUMLIT --tag as NUMLIT
        end
    end

    -- State DIGDOT: we are in a NUMLIT, and we have seen ".".
    local function handle_DIGDOT()
        if isDigit(ch) then
            add1()
        else
          state = DONE --move to handle_DONE
          category = lexit.NUMLIT --tag as NUMLIT
        end
    end

    -- State EXPONENT: we are in a NUMLIT, and we have seen "e".
    local function handle_EXPONENT()
        if isDigit(ch) then
            add1()
        else
            state = DONE --move to handle_DONE
            category = lexit.NUMLIT --tag as NUMLIT
        end
    end

    -- State PUNCT: we are in a PUNCT.
    local function handle_PUNCT()
        if ch == "=" and (lexstr:sub(1,1) ~= "~") then
           --add1()
            state = OP --move to handle_DONE
            category = lexit.OP --tag as OP
        else
            state = DONE --move to handle_DONE
            category = lexit.PUNCT --tag as PUNCT
        end
    end

    -- State String: We are in a string
    local function handle_STRING()
        if ch == lexstr:sub(1,1) then
            add1()
            state = DONE --move to handle_DONE
            category = lexit.STRLIT --tag as STRLIT
        elseif ch == "" or ch == "\n" then
            state = DONE --move to handle_DONE
            category = lexit.MAL --tag as MAL
        else
            add1()
        end
    end

    -- State OP: We have seen an OP and have seen "=, <, >, +, -, *, /, %, [, or ]"
    local function handle_OP()
        if ch == "=" and (lexstr:sub(1,1) == "=" or 
        lexstr:sub(1,1) == "<" or lexstr:sub(1,1) == ">" or lexstr:sub(1,1) == "!") then
            add1()
            state = DONE --move to handle_DONE
            category = lexit.OP --tag as OP
        else
            state = DONE --move to handle_DONE
            category = lexit.OP --tag as OP
        end
    end

    -- ***** Table of State-Handler Functions *****

    handlers = {
        [DONE]      =handle_DONE,
        [START]     =handle_START,
        [LETTER]    =handle_LETTER,
        [DIGIT]     =handle_DIGIT,
        [DIGDOT]    =handle_DIGDOT,
        [STRING]    =handle_STRING,
        [OP]        =handle_OP,
        [EXPONENT]  =handle_EXPONENT,
        [PUNCT]     =handle_PUNCT,
        [MAL]       =handle_DONE,
    }

    -- ***** Iterator Function *****

    -- getLexeme
    -- Called each time through the for-in loop.
    -- Returns a pair: lexeme-string (string) and category (int), or
    -- nil, nil if no more lexemes.
    local function getLexeme()
        if pos > program:len() then
            return nil, nil
        end
        lexstr = ""
        state = START
        while state ~= DONE do
            ch = currChar()
            handlers[state]()
        end

        skipToNextLexeme()
        return lexstr, category
    end

    -- ***** Body of Function lex *****

    -- Initialize & return the iterator function
    pos = 1
    skipToNextLexeme()
    return getLexeme
end

return lexit