local lpeg = require "lpeglabel"
local T = lpeg.T

local errorList = {}
local errorIndex = {}
local errorMessage = {}

local function getmessage(errid)
	return errorMessage[errid]
	    or "unknown error ("..errid..")"
end

local module = { getmessage = getmessage }

function module.newerror(id, msg)
	local index = #errorList+1
	errorList[index] = id
	errorIndex[id] = index
	errorMessage[id] = msg
end

function module.getmsgbyidx(index)
	local errid = errorList[index]
	if errid == nil then
		return "unknown error index ("..index..")"
	end
	return getmessage(errid)
end

function module.throw(errid)
  local index = assert(errorIndex[errid], "error not declared")
  return T(index)
end

return module
