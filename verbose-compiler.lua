local lpeg = require "lpeglabel"

lpeg.locale(lpeg)

local P, S, V = lpeg.P, lpeg.S, lpeg.V
local C, Carg, Cb, Cc = lpeg.C, lpeg.Carg, lpeg.Cb, lpeg.Cc
local Cf, Cg, Cmt, Cp, Cs, Ct = lpeg.Cf, lpeg.Cg, lpeg.Cmt, lpeg.Cp, lpeg.Cs, lpeg.Ct
local Lc, T = lpeg.Lc, lpeg.T

local alpha, digit, alnum = lpeg.alpha, lpeg.digit, lpeg.alnum
local xdigit = lpeg.xdigit
local space = lpeg.space

local function VerboseLine(flag, ...)
	if flag == "" then flag = '"default"' end
	local verbdata = table.concat({...}, ",")
	return 'io.write("[",'..flag..',"] ",'..verbdata..',"\\n")'
end

local function RetStat(expr, ...)
	local verbstats = {}
	local others = {}
	for i=1, select("#", ...) do
		local value = select(i, ...)
		if value.kind == "Verbose" then
			verbstats[#verbstats+1] = value[1]
		else
			others[#others+1] = value[1]
		end
	end
	return 'return(function(...) '
	     ..table.concat(verbstats," ")
	     ..' return ... end)('..expr..")"
	     ..table.concat(others)
end

local lua53 = require "lua-parser.grammar"

local function fill(src, dst)
  for key, value in pairs(src) do
    if dst[key] == nil then
      dst[key] = value
    end
  end
  return dst
end

local grammar = lua53.grammar
local G = fill(grammar, { Cs(V"Lua"),
	Block   = V"InterStatement"
	        * (V"Statement" * V"InterStatement")^0
	        * (V"RetStat" / RetStat)^-1;
	RetStat = V"ReturnCmd"
	        * C(V"RetExpr")
	        * V"Semicolon"^-1
	        * ( Ct(Cg(Cc"Space", "kind") * C(V"Space"))
	          + Ct(Cg(Cc"Verbose", "kind") * (V"VerboseLine" / VerboseLine))
	          + Ct(Cg(Cc"Comment", "kind") * C(V"Comment"))
	          )^0;
	RetExpr = V"Skip"
	        * (V"Expression" * (V"Skip" * V"Comma" * V"Skip" * V"Expression")^0)^-1;

	InterStatement = ( V"Space"
	                 + V"VerboseLine" / VerboseLine
	                 + V"Comment"
	                 )^0;

	VerboseLine = P"-->"
		        * C(V"Name"^-1)
		        * P">"
		        * space^0
		        * V"VerboseData"^0;
	VerboseData = V"VerboseExpr"
	            + V"VerboseMesg";
	VerboseExpr = P"$" * C(V"Name")
	            + P"$(" * C(V"Expression") * ")";
	VerboseMesg = Cs(Cc'"' * V"VerbMsgChar"^1 * Cc'"');
	VerbMsgChar = P'"' / [[\"]]
	            + P"$$" / "$"
	            + (P(1) - S"$\n");
})

local validator = require("lua-parser.validator")
local syntaxerror = validator.syntaxerror

local source = assert(io.read("*a"))

local errorinfo = { subject = source, filename = "stdio" }
lpeg.setmaxstack(1000)
local output, erridx, sfail = lpeg.match(G, source, nil, errorinfo)

if not output then
	local errpos = #source-#sfail+1
	local errmsg = lua53.errors[erridx][2]
	error(syntaxerror(errorinfo, errpos, errmsg))
end

io.write(output)
print()