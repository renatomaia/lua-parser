local lpeg = require "lpeglabel"

lpeg.locale(lpeg)

local P, S, V = lpeg.P, lpeg.S, lpeg.V
local C, Carg, Cb, Cc = lpeg.C, lpeg.Carg, lpeg.Cb, lpeg.Cc
local Cf, Cg, Cmt, Cp, Cs, Ct = lpeg.Cf, lpeg.Cg, lpeg.Cmt, lpeg.Cp, lpeg.Cs, lpeg.Ct
local Lc, T = lpeg.Lc, lpeg.T

local alpha, digit, alnum = lpeg.alpha, lpeg.digit, lpeg.alnum
local xdigit = lpeg.xdigit
local space = lpeg.space

local QuoteOther = {
	["'"] = '"',
	['"'] = "'",
}
local QuoteAvoided = '"'
local QuotePrefered = QuoteOther[QuoteAvoided]
function QuoteString(value)
	local quote, data = string.sub(value, 1, 1), string.sub(2, -2)
	if quote == QuoteAvoided then
		data = string.gsub(data, QuotePrefered, [[\]]..QuotePrefered)
		data = string.gsub(data, [[\]]..QuoteAvoided, QuoteAvoided)
		return QuotePrefered..data..QuotePrefered
	end
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

local function Ctag(patt, name)
  return Ct(Cg(Cp(), "pos") * Cg(Cc(name), "tag") * C(patt))
end

local grammar = lua53.grammar
local G = fill(grammar, { Ct(V"Lua"),
	Chunk = Cg(grammar.Chunk, "Chunk"),
	ShebangCmd = Cg(grammar.ShebangCmd, "Shebang"),

	Block = Ct(grammar.Block),
	Expression = Ct(grammar.Expression),

	IfStat = Ctag(grammar.IfStat, "If")
	IfPart = Cg(Vs"IfCmd" * expect(Vs"Expression", "ExprIf"), "condition")
	       * Cg(expect(V"ThenCmd", "ThenIf") * V"Block", "thenbody")

	Space = V"SpcInline"^1
	      + V"SpcCloseLn" * V"SpcBlankLn"^-1 * V"SpcOpenLn",
	SpcBlankLn = V"SpcCloseLn"^1,
	SpcCloseLn = V"SpcInline"^0 * V"SpcLnBrk",
	SpcOpenLn = V"SpcInline"^0,
	SpcInline = space - V"SpcLnBrk",
	SpcLnBrk = P"\n" * P"\r"^-1
	         + P"\r";

	--QuoteString = grammar.QuoteString / QuoteString;
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

local Viewer = require "loop.debug.Viewer"

Viewer:write(output)
print()