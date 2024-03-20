-- lexit.lua
-- Ash Moore-Schultz
-- 02-20-2024
-- For CS 331 Spring 2024
-- Assignment 3 Lexer Module

--- Set up module table
local lexit = {}  -- The module

-- Public Constants
lexit.KEY    = 1
lexit.ID     = 2
lexit.NUMLIT = 3
lexit.STRLIT = 4
lexit.OP     = 5
lexit.PUNCT  = 6
lexit.MAL    = 7

--Lexeme categories that can be read by humans
lexit.catnames = {
    "Keyword",
    "Identifier",
    "NumericLiteral",
    "StringLiteral",
    "Operator",
    "Punctuation",
    "Malformed"
}



--****Character Functions****

-- isLetter
-- Checks if a character is a letter
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
-- Checks if string c is a digit
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
-- Checks if string c is a whitespace.
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
-- Checks if string c is ASCII
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
-- Checks if if string c is an illegal character.
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

-- The Lexer
function lexit.lex(program)

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

    local DONE      = 0
    local START     = 1
    local LETTER    = 2
    local DIGIT     = 3
	local DIGDOT	= 4
    local PUNCT 	= 5
    local OP     	= 6
    local EXPONENT  = 7
    local STRLIT    = 8
	local MAL		= 9

    -- ***** Character-Related Utility Functions *****

    -- currChar
    local function currChar()
        return program:sub(pos, pos)
    end

    -- nextChar
    local function nextChar()
        return program:sub(pos+1, pos+1)
    end

	local function nextCharAt(position)
		return program:sub(pos+position, pos+position)
	end

    -- drop1
    local function drop1()
        pos = pos+1
    end

    -- add1
    local function add1()
        lexstr = lexstr .. currChar()
        drop1()
    end

    -- skipToNextLexeme
	--skips whitespace and comments that start with #
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
            while currChar() ~= "\n" do
                if currChar() == "" then  -- End of input?
                   return
                end
                drop1()  -- Drop character inside comment
            end
        end
    end

    -- ***** State-Handler Functions *****

    local function handle_DONE()
        error("'DONE' state should not be handled\n")
    end

        -- State START: no character read yet.
		local function handle_START()
			if isLetter(ch) or ch == "_" then
				add1()
				state = LETTER
			elseif isDigit(ch) then
				add1()
				state = DIGIT
			elseif ch == "'" or ch == '"' then
				add1()
				state = STRLIT
			elseif ch == "=" or ch == "<" or ch == ">" or ch == "+" or ch == "-" or ch == "*" or ch == "/" or ch == "%" or ch == "[" or ch == "]" then
				add1()
				state = OP
			elseif not isIllegal(ch) then
				add1()
				state = PUNCT
			else
				add1()
				state = DONE
				category = lexit.MAL
			end
		end

    -- State LETTER: we are in an ID.
    local function handle_LETTER()
        if isLetter(ch) or isDigit(ch) or ch == "_" then
            add1()
        else
            state = DONE
            if lexstr == "true" or lexstr == "while"
			or lexstr == "print" or lexstr == "and"
			or lexstr == "char" or lexstr == "def"
			or lexstr == "eol" or lexstr == "else" 
			or lexstr == "elseif" or lexstr == "false" 
			or lexstr == "if" or lexstr == "inputnum"
			or lexstr == "not" or lexstr == "or" or lexstr == "rand"
			or lexstr == "output" or lexstr == "return"
			then
                category = lexit.KEY
            else
                category = lexit.ID
            end
        end
    end

    -- State DIGIT: we are in a NUMLIT, and we have NOT seen ".".
    local function handle_DIGIT()
        if isDigit(ch) then
            add1()
        elseif ch == "." then
			state = DIGDOT
        elseif ch == "e" or ch == "E" then
            if nextChar() == "+" then
                if isDigit(nextCharAt(2)) then
                   add1()
                   add1()
                   state = EXPONENT
                else
                    state = DONE
                    category = lexit.NUMLIT
                end
            elseif isDigit(nextChar()) then
                add1()
                state = EXPONENT
            else
                state = DONE
                category = lexit.NUMLIT
            end
        -- elseif ch == "+" then
        --     if isDigit(nextChar()) then
        --         add1()
        --     else
        --         state = DONE
        --         category = lexit.NUMLIT
        --     end
        else
            state = DONE
            category = lexit.NUMLIT
        end
    end
	  -- State DIGDOT: we are in a NUMLIT, and we have seen ".".
	  local function handle_DIGDOT()
        if isDigit(ch) then
            add1()
        else
            state = DONE
            category = lexit.NUMLIT
        end
    end

    local function handle_EXPONENT()
        if isDigit(ch) then
            add1()
        else
            state = DONE
            category = lexit.NUMLIT
        end
    end

-- State STRLIT: we have seen a quote (single or double) and nothing else.
local function handle_STRLIT()
	if ch == lexstr:sub(1,1) then
		add1() --Include closing quotes in lexeme
		state = DONE
		category = lexit.STRLIT
	elseif ch == "" or ch == "\n" then
		state = DONE
		category = lexit.MAL
	else
		add1()
	end
end
   -- State OP: we are in an OP.
   local function handle_OP()
	if ch == "=" and (lexstr:sub(1,1) == "=" or lexstr:sub(1,1) == "<" or lexstr:sub(1,1) == ">" or lexstr:sub(1,1) == "!") then
		add1()
		state = DONE
		category = lexit.OP
	else
		state = DONE
		category = lexit.OP
	end
end

-- State PUNCT: we are in a PUNCT.
local function handle_PUNCT()
	--if current character is = or ~ then add to lexeme
	if ch == "=" and (lexstr:sub(1,1) ~= "~") then
		-- add1()
		state = OP
		category = lexit.OP
	else
		--otherwise send it to punctuation
		state = DONE
		category = lexit.PUNCT
	end
end

    -- ***** Table of State-Handler Functions *****

    handlers = {
        [DONE]=handle_DONE,
        [START]=handle_START,
        [LETTER]=handle_LETTER,
        [DIGIT]=handle_DIGIT,
		[DIGDOT]=handle_DIGDOT,
        [PUNCT]= handle_PUNCT,
        [OP]=handle_OP,
        [EXPONENT]=handle_EXPONENT,
        [STRLIT]=handle_STRLIT
    }

    -- ***** Iterator Function *****

    -- getLexeme
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
    pos = 1
    skipToNextLexeme()
    return getLexeme
end


-- Module Table Return

return lexit

