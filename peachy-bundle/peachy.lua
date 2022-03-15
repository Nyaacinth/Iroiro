-- Bundled by luabundle {"version":"1.6.0"}
local __bundle_require, __bundle_loaded, __bundle_register, __bundle_modules = (function(superRequire)
	local loadingPlaceholder = {[{}] = true}

	local register
	local modules = {}

	local require
	local loaded = {}

	register = function(name, body)
		if not modules[name] then
			modules[name] = body
		end
	end

	require = function(name)
		local loadedModule = loaded[name]

		if loadedModule then
			if loadedModule == loadingPlaceholder then
				return nil
			end
		else
			if not modules[name] then
				if not superRequire then
					local identifier = type(name) == 'string' and '\"' .. name .. '\"' or tostring(name)
					error('Tried to require ' .. identifier .. ', but no such module has been registered')
				else
					return superRequire(name)
				end
			end

			loaded[name] = loadingPlaceholder
			loadedModule = modules[name](require, loaded, register, modules)
			loaded[name] = loadedModule
		end

		return loadedModule
	end

	return require, loaded, register, modules
end)(require)
__bundle_register("__root", function(require, _LOADED, __bundle_register, __bundle_modules)
--- A parser/renderer for Aseprite animations in LÖVE.
-- @classmod peachy

local peachy = {
  _VERSION = "1.0.0-alpha",
  _DESCRIPTION = "A parser/renderer for Aseprite animations in LÖVE.",
  _URL = "https://github.com/josh-perry/peachy",
  _LICENSE = [[
    MIT License

    Copyright (c) 2018 Josh Perry

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
  ]]
}

local json = require("lib.json")
local cron = require("lib.cron")

peachy.__index = peachy

--- Creates a new Peachy animation object.
  --
  -- If imageData isn't specified then Peachy will attempt to load it using the
  -- filename from the JSON data.
  --
  -- If no initial tag is set then the object will be paused (i.e. not displayed) with no tag.
  -- The animation will start playing immediately once created.
  --
  -- @usage
  -- -- Load the image ourselves and set animation tag to "Spin".
  -- -- Will start playing immediately.
  -- spinner = peachy.new("spinner.json", love.graphics.newImage("spinner.png"), "Spin")
  --
  -- @tparam string dataFile a path to an Aseprite JSON file. It is also possible to pass a predecoded table,
  -- which is useful for performance when creating large amounts of the same animation.
  -- @tparam Image imageData a LÖVE image  to animate.
  -- @tparam string initialTag the name of the animation tag to use initially.
  -- @return the new Peachy object.
  function peachy.new(dataFile, imageData, initialTag)
    assert(dataFile ~= nil, "No JSON data!")

    local self = setmetatable({}, peachy)

    --store the path to the passed json file
    self.json_path = dataFile

    -- check if datafile is a lua table (i.e. pre decoded)
    if type(dataFile) == 'table' then
      self._jsonData = dataFile
    else
      -- Read the data
      self._jsonData = json.decode(love.filesystem.read(dataFile))
    end

  -- Load the image
  self.image = imageData or love.graphics.newImage(self._jsonData.meta.image)

  self:_checkImageSize()

  self:_initializeFrames()
  self:_initializeTags()

  self.paused = true

  self.tag = nil
  self.tagName = nil
  self.direction = nil

  if initialTag then
    self:setTag(initialTag)
    self.paused = false
  end

  return self
end

--- Switch to a different animation tag.
-- In the case that we're attempting to switch to the animation currently playing,
-- nothing will happen.
--
-- @tparam string tag the animation tag name to switch to.
function peachy:setTag(tag)
  assert(tag, "No animation tag specified!")
  assert(self.frameTags[tag], "Tag "..tag.." not found in frametags!")

  if self.tag == self.frameTags[tag] then
    return
  end

  self.tagName = tag
  self.tag = self.frameTags[self.tagName]
  self.frameIndex = nil
  self.direction = self.tag.direction

  if self.direction == "pingpong" then
    self.direction = "forward"
  end

  self:nextFrame()
end

--- Jump to a particular frame index (1-based indexes) in the current animation.
--
-- Errors if the frame is outside the tag's frame range.
--
-- @usage
-- -- Go to the 4th frame
-- sound:setFrame(4)
--
-- @tparam number frame the frame index to jump to.
function peachy:setFrame(frame)
  if frame < 1 or frame > #self.tag.frames then
    error("Frame "..frame.." is out of range of tag '"..self.tagName.."' (1.."..#self.tag.frames..")")
  end

  self.frameIndex = frame

  self.frame = self.tag.frames[self.frameIndex]
  self.frameTimer = cron.after(self.frame.duration, self.nextFrame, self)
end

--- Get the current frame of the current animation
-- @usage
-- Get the 2nd frame
-- local f = sound:getFrame()
--
function peachy:getFrame()
  return self.frameIndex
end

--- Get the json path passed in the object
-- @usage
-- Get the (string) JSON path
-- local str_json = obj:getJSON()
--
function peachy:getJSON()
  return self.json_path
end

--- Draw the animation's current frame in a specified location.
-- @tparam number x the x position.
-- @tparam number y the y position.
-- @tparam number rot the rotation to draw at.
-- @tparam number sx the x scaling.
-- @tparam number sy the y scaling.
-- @tparam number ox the origin offset x.
-- @tparam number oy the origin offset y.
function peachy:draw(x, y, rot, sx, sy, ox, oy)
  if not self.frame then
    return
  end

  love.graphics.draw(self.image, self.frame.quad, x, y, rot or 0, sx or 1, sy or 1, ox or 0, oy or 0)
end

--- Update the animation.
-- @tparam number dt frame delta. Should be called from love.update and given the dt.
function peachy:update(dt)
  assert(dt, "No dt passed into update!")

  if self.paused then
    return
  end

  -- If we're trying to play an animation and it's nil or hasn't been set up
  -- properly then error
  assert(self.tag, "No animation tag has been set!")
  assert(self.frameTimer, "Frame timer hasn't been initialized!")

  -- Update timer in milliseconds since that's how Aseprite stores durations
  self.frameTimer:update(dt * 1000)
end

--- Move to the next frame.
-- Internal: unless you want to skip frames, this generally will not ever
-- need to be called manually.
function peachy:nextFrame()
  local forward = self.direction == "forward"

  if forward then
    self.frameIndex = (self.frameIndex or 0) + 1
  else
    self.frameIndex = (self.frameIndex or #self.tag.frames + 1) - 1
  end

  -- Looping
  if forward and self.frameIndex > #self.tag.frames then
    if self.tag.direction == "pingpong" then
      self:_pingpongBounce()
    else
      self.frameIndex = 1
    end
    self:call_onLoop()
  elseif not forward and self.frameIndex < 1 then
    if self.tag.direction == "pingpong" then
      self:_pingpongBounce()
    else
      self.frameIndex = #self.tag.frames
      self:call_onLoop()
    end
  end

  -- Get next frame
  self.frame = self.tag.frames[self.frameIndex]

  self.frameTimer = cron.after(self.frame.duration, self.nextFrame, self)
end

--- Check for callbacks
function peachy:call_onLoop()
  if self.callback_onLoop then self.callback_onLoop(unpack(self.args_onLoop)) end
end

--- Pauses the animation.
function peachy:pause()
  self.paused = true
end

--- Unpauses the animation.
function peachy:play()
  self.paused = false
end

--- Stops the animation (pause it then return to first frame or last if specified)
function peachy:stop(onLast)
  local index = 1
  self.paused = true
  if onLast then index = #self.tag.frames end
  self:setFrame(index)
end

--- Adds a callback function that will be called when the animation loops
function peachy:onLoop(fn, ...)
  self.callback_onLoop = fn
  self.args_onLoop = {...}
end

--- Toggle between playing/paused.
function peachy:togglePlay()
  if self.paused then
    self:play()
  else
    self:pause()
  end
end

--- Provides width stored in the metadata of a current frame
function peachy:getWidth()
    return self._jsonData.frames[self.frameIndex].frame.w
end

--- Provides height stored in the metadata of a current frame
function peachy:getHeight()
    return self._jsonData.frames[self.frameIndex].frame.h
end

--- Provides dimensions stored in the metadata of a current frame
function peachy:getDimensions()
    return self:getWidth(), self:getHeight()
end

--- Internal: handles the ping-pong animation type.
--
-- Should only be called when we actually want to bounce.
-- Swaps the direction.
function peachy:_pingpongBounce()
  -- We need to increment/decrement frame index by 2 because
  -- at this point we've already gone to the next frame
  if self.direction == "forward" then
    self.direction = "reverse"
    self.frameIndex = self.frameIndex - 2
  else
    self.direction = "forward"
    self.frameIndex = self.frameIndex + 2
  end
end

--- Internal: loads all of the frames
--
-- Loads quads and frame duration data from the JSON.
--
-- Called from peachy.new
function peachy:_initializeFrames()
  assert(self._jsonData ~= nil, "No JSON data!")
  assert(self._jsonData.meta ~= nil, "No metadata in JSON!")
  assert(self._jsonData.frames ~= nil, "No frame data in JSON!")

  -- Initialize all the quads
  self.frames = {}
  for _, frameData in ipairs(self._jsonData.frames) do
    local frame = {}

    local fd = frameData.frame
    frame.quad = love.graphics.newQuad(fd.x, fd.y, fd.w, fd.h, self._jsonData.meta.size.w, self._jsonData.meta.size.h)
    frame.duration = frameData.duration

    table.insert(self.frames, frame)
  end
end

--- Internal: loads all of the animation tags
--
-- Called from peachy.new
function peachy:_initializeTags()
  assert(self._jsonData ~= nil, "No JSON data!")
  assert(self._jsonData.meta ~= nil, "No metadata in JSON!")
  assert(self._jsonData.meta.frameTags ~= nil, "No frame tags in JSON! Make sure you exported them in Aseprite!")

  self.frameTags = {}

  for _, frameTag in ipairs(self._jsonData.meta.frameTags) do
    local ft = {}
    ft.direction = frameTag.direction
    ft.frames = {}

    for frame = frameTag.from + 1, frameTag.to + 1 do
      table.insert(ft.frames, self.frames[frame])
    end

    self.frameTags[frameTag.name] = ft
  end
end

--- Internal: checks that the loaded image size matches the metadata
--
-- Called from peachy.new
function peachy:_checkImageSize()
  local imageWidth, imageHeight = self._jsonData.meta.size.w, self._jsonData.meta.size.h
  assert(imageWidth == self.image:getWidth(), "Image width metadata doesn't match actual width of file")
  assert(imageHeight == self.image:getHeight(), "Image height metadata doesn't match actual height of file")
end

return peachy

end)
__bundle_register("lib.cron", function(require, _LOADED, __bundle_register, __bundle_modules)
local cron = {
  __VERSION     = 'cron.lua 2.0.0',
  __DESCRIPTION = 'Time-related functions for lua',
  __URL         = 'https://github.com/kikito/cron.lua',
  __LICENSE     = [[
    Copyright (c) 2011 Enrique García Cota

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
  ]]
}

-- Private functions

local function isCallable(callback)
  local tc = type(callback)
  if tc == 'function' then return true end
  if tc == 'table' then
    local mt = getmetatable(callback)
    return type(mt) == 'table' and type(mt.__call) == 'function'
  end
  return false
end

local function checkPositiveInteger(name, value)
  if type(value) ~= "number" or value < 0 then
    error(name .. " must be a positive number")
  end
end

local Clock = {}
local Clock_mt = {__index = Clock}

local function newClock(time, callback, update, ...)
  checkPositiveInteger('time', time)
  assert(isCallable(callback), "callback must be a function")

  return setmetatable({
    time     = time,
    callback = callback,
    args     = {...},
    running  = 0,
    update   = update
  }, Clock_mt)
end

local function updateAfterClock(self, dt) -- returns true if expired
  checkPositiveInteger('dt', dt)

  if self.running >= self.time then return true end

  self.running = self.running + dt

  if self.running >= self.time then
    self.callback(unpack(self.args))
    return true
  end
  return false
end

local function updateEveryClock(self, dt)
  checkPositiveInteger('dt', dt)

  self.running = self.running + dt

  while self.running >= self.time do
    self.callback(unpack(self.args))
    self.running = self.running - self.time
  end
  return false
end

function Clock:reset(running)
  running = running or 0
  checkPositiveInteger('running', running)

  self.running = running
end


function cron.after(time, callback, ...)
  return newClock(time, callback, updateAfterClock, ...)
end

function cron.every(time, callback, ...)
  return newClock(time, callback, updateEveryClock, ...)
end

return cron


end)
__bundle_register("lib.json", function(require, _LOADED, __bundle_register, __bundle_modules)
--
-- json.lua
--
-- Copyright (c) 2018 rxi
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy of
-- this software and associated documentation files (the "Software"), to deal in
-- the Software without restriction, including without limitation the rights to
-- use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is furnished to do
-- so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
--

local json = { _version = "0.1.1" }

-------------------------------------------------------------------------------
-- Encode
-------------------------------------------------------------------------------

local encode

local escape_char_map = {
  [ "\\" ] = "\\\\",
  [ "\"" ] = "\\\"",
  [ "\b" ] = "\\b",
  [ "\f" ] = "\\f",
  [ "\n" ] = "\\n",
  [ "\r" ] = "\\r",
  [ "\t" ] = "\\t",
}

local escape_char_map_inv = { [ "\\/" ] = "/" }
for k, v in pairs(escape_char_map) do
  escape_char_map_inv[v] = k
end


local function escape_char(c)
  return escape_char_map[c] or string.format("\\u%04x", c:byte())
end


local function encode_nil(val)
  return "null"
end


local function encode_table(val, stack)
  local res = {}
  stack = stack or {}

  -- Circular reference?
  if stack[val] then error("circular reference") end

  stack[val] = true

  if val[1] ~= nil or next(val) == nil then
    -- Treat as array -- check keys are valid and it is not sparse
    local n = 0
    for k in pairs(val) do
      if type(k) ~= "number" then
        error("invalid table: mixed or invalid key types")
      end
      n = n + 1
    end
    if n ~= #val then
      error("invalid table: sparse array")
    end
    -- Encode
    for i, v in ipairs(val) do
      table.insert(res, encode(v, stack))
    end
    stack[val] = nil
    return "[" .. table.concat(res, ",") .. "]"

  else
    -- Treat as an object
    for k, v in pairs(val) do
      if type(k) ~= "string" then
        error("invalid table: mixed or invalid key types")
      end
      table.insert(res, encode(k, stack) .. ":" .. encode(v, stack))
    end
    stack[val] = nil
    return "{" .. table.concat(res, ",") .. "}"
  end
end


local function encode_string(val)
  return '"' .. val:gsub('[%z\1-\31\\"]', escape_char) .. '"'
end


local function encode_number(val)
  -- Check for NaN, -inf and inf
  if val ~= val or val <= -math.huge or val >= math.huge then
    error("unexpected number value '" .. tostring(val) .. "'")
  end
  return string.format("%.14g", val)
end


local type_func_map = {
  [ "nil"     ] = encode_nil,
  [ "table"   ] = encode_table,
  [ "string"  ] = encode_string,
  [ "number"  ] = encode_number,
  [ "boolean" ] = tostring,
}


encode = function(val, stack)
  local t = type(val)
  local f = type_func_map[t]
  if f then
    return f(val, stack)
  end
  error("unexpected type '" .. t .. "'")
end


function json.encode(val)
  return ( encode(val) )
end


-------------------------------------------------------------------------------
-- Decode
-------------------------------------------------------------------------------

local parse

local function create_set(...)
  local res = {}
  for i = 1, select("#", ...) do
    res[ select(i, ...) ] = true
  end
  return res
end

local space_chars   = create_set(" ", "\t", "\r", "\n")
local delim_chars   = create_set(" ", "\t", "\r", "\n", "]", "}", ",")
local escape_chars  = create_set("\\", "/", '"', "b", "f", "n", "r", "t", "u")
local literals      = create_set("true", "false", "null")

local literal_map = {
  [ "true"  ] = true,
  [ "false" ] = false,
  [ "null"  ] = nil,
}


local function next_char(str, idx, set, negate)
  for i = idx, #str do
    if set[str:sub(i, i)] ~= negate then
      return i
    end
  end
  return #str + 1
end


local function decode_error(str, idx, msg)
  local line_count = 1
  local col_count = 1
  for i = 1, idx - 1 do
    col_count = col_count + 1
    if str:sub(i, i) == "\n" then
      line_count = line_count + 1
      col_count = 1
    end
  end
  error( string.format("%s at line %d col %d", msg, line_count, col_count) )
end


local function codepoint_to_utf8(n)
  -- http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=iws-appendixa
  local f = math.floor
  if n <= 0x7f then
    return string.char(n)
  elseif n <= 0x7ff then
    return string.char(f(n / 64) + 192, n % 64 + 128)
  elseif n <= 0xffff then
    return string.char(f(n / 4096) + 224, f(n % 4096 / 64) + 128, n % 64 + 128)
  elseif n <= 0x10ffff then
    return string.char(f(n / 262144) + 240, f(n % 262144 / 4096) + 128,
                       f(n % 4096 / 64) + 128, n % 64 + 128)
  end
  error( string.format("invalid unicode codepoint '%x'", n) )
end


local function parse_unicode_escape(s)
  local n1 = tonumber( s:sub(3, 6),  16 )
  local n2 = tonumber( s:sub(9, 12), 16 )
  -- Surrogate pair?
  if n2 then
    return codepoint_to_utf8((n1 - 0xd800) * 0x400 + (n2 - 0xdc00) + 0x10000)
  else
    return codepoint_to_utf8(n1)
  end
end


local function parse_string(str, i)
  local has_unicode_escape = false
  local has_surrogate_escape = false
  local has_escape = false
  local last
  for j = i + 1, #str do
    local x = str:byte(j)

    if x < 32 then
      decode_error(str, j, "control character in string")
    end

    if last == 92 then -- "\\" (escape char)
      if x == 117 then -- "u" (unicode escape sequence)
        local hex = str:sub(j + 1, j + 5)
        if not hex:find("%x%x%x%x") then
          decode_error(str, j, "invalid unicode escape in string")
        end
        if hex:find("^[dD][89aAbB]") then
          has_surrogate_escape = true
        else
          has_unicode_escape = true
        end
      else
        local c = string.char(x)
        if not escape_chars[c] then
          decode_error(str, j, "invalid escape char '" .. c .. "' in string")
        end
        has_escape = true
      end
      last = nil

    elseif x == 34 then -- '"' (end of string)
      local s = str:sub(i + 1, j - 1)
      if has_surrogate_escape then
        s = s:gsub("\\u[dD][89aAbB]..\\u....", parse_unicode_escape)
      end
      if has_unicode_escape then
        s = s:gsub("\\u....", parse_unicode_escape)
      end
      if has_escape then
        s = s:gsub("\\.", escape_char_map_inv)
      end
      return s, j + 1

    else
      last = x
    end
  end
  decode_error(str, i, "expected closing quote for string")
end


local function parse_number(str, i)
  local x = next_char(str, i, delim_chars)
  local s = str:sub(i, x - 1)
  local n = tonumber(s)
  if not n then
    decode_error(str, i, "invalid number '" .. s .. "'")
  end
  return n, x
end


local function parse_literal(str, i)
  local x = next_char(str, i, delim_chars)
  local word = str:sub(i, x - 1)
  if not literals[word] then
    decode_error(str, i, "invalid literal '" .. word .. "'")
  end
  return literal_map[word], x
end


local function parse_array(str, i)
  local res = {}
  local n = 1
  i = i + 1
  while 1 do
    local x
    i = next_char(str, i, space_chars, true)
    -- Empty / end of array?
    if str:sub(i, i) == "]" then
      i = i + 1
      break
    end
    -- Read token
    x, i = parse(str, i)
    res[n] = x
    n = n + 1
    -- Next token
    i = next_char(str, i, space_chars, true)
    local chr = str:sub(i, i)
    i = i + 1
    if chr == "]" then break end
    if chr ~= "," then decode_error(str, i, "expected ']' or ','") end
  end
  return res, i
end


local function parse_object(str, i)
  local res = {}
  i = i + 1
  while 1 do
    local key, val
    i = next_char(str, i, space_chars, true)
    -- Empty / end of object?
    if str:sub(i, i) == "}" then
      i = i + 1
      break
    end
    -- Read key
    if str:sub(i, i) ~= '"' then
      decode_error(str, i, "expected string for key")
    end
    key, i = parse(str, i)
    -- Read ':' delimiter
    i = next_char(str, i, space_chars, true)
    if str:sub(i, i) ~= ":" then
      decode_error(str, i, "expected ':' after key")
    end
    i = next_char(str, i + 1, space_chars, true)
    -- Read value
    val, i = parse(str, i)
    -- Set
    res[key] = val
    -- Next token
    i = next_char(str, i, space_chars, true)
    local chr = str:sub(i, i)
    i = i + 1
    if chr == "}" then break end
    if chr ~= "," then decode_error(str, i, "expected '}' or ','") end
  end
  return res, i
end


local char_func_map = {
  [ '"' ] = parse_string,
  [ "0" ] = parse_number,
  [ "1" ] = parse_number,
  [ "2" ] = parse_number,
  [ "3" ] = parse_number,
  [ "4" ] = parse_number,
  [ "5" ] = parse_number,
  [ "6" ] = parse_number,
  [ "7" ] = parse_number,
  [ "8" ] = parse_number,
  [ "9" ] = parse_number,
  [ "-" ] = parse_number,
  [ "t" ] = parse_literal,
  [ "f" ] = parse_literal,
  [ "n" ] = parse_literal,
  [ "[" ] = parse_array,
  [ "{" ] = parse_object,
}


parse = function(str, idx)
  local chr = str:sub(idx, idx)
  local f = char_func_map[chr]
  if f then
    return f(str, idx)
  end
  decode_error(str, idx, "unexpected character '" .. chr .. "'")
end


function json.decode(str)
  if type(str) ~= "string" then
    error("expected argument of type string, got " .. type(str))
  end
  local res, idx = parse(str, next_char(str, 1, space_chars, true))
  idx = next_char(str, idx, space_chars, true)
  if idx <= #str then
    decode_error(str, idx, "trailing garbage")
  end
  return res
end


return json

end)
return __bundle_require("__root")
