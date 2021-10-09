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
--
-- Dependencies

local lume = require("narrator.libs.lume")
local enums = require("narrator.enums")
local parser = require("narrator.parser")
local Story = require("narrator.story")

--
-- Private

local folderSeparator = package.config:sub(1, 1)

--- Clears path from '.lua' and '.ink' extensions and replace '.' to '/' or '\'
-- @param path string: path to clear
-- @return string: a clean path
local function clearPath(path)
  local path = path:gsub('.lua$', '')
  local path = path:gsub('.ink$', '')

  if path:match('%.') and not path:match(folderSeparator) then
    path = path:gsub('%.', folderSeparator)
  end

  return path
end

--- Parse an Ink file to a string content
-- @param path string: path to an Ink file
-- @return string: a content string
local function readFile(path)
  local path = clearPath(path) .. '.ink'

  local file = io.open(path, 'r')
  assert(file, 'File doesn\'t exist: ' .. path)

  local content = file:read('*all')
  file:close()

  return content
end

--- Save a book to lua module
-- @param book table: a book
-- @param path string: a path to save
-- @return boolean: success
local function saveBook(book, path)
  local path = clearPath(path)  .. '.lua'

  local data = lume.serialize(book)
  data = data:gsub('%[%d+%]=', '')
  data = data:gsub('[\"[%w_]+\"]', function(match) return
    match:sub(3, #match - 2)
  end)
  
  local file = io.open(path, 'w')
  if file == nil then
    return false
  end
  
  file:write('return ' .. data)
  file:close()
  
  return true
end

--- Merge a chapter to a book
-- @param book table: a parent book
-- @param chapter table: a chapter to merge
-- @return table: a book
local function merge(book, chapter)
  -- Check a engine version compatibility
  if chapter.version.engine and chapter.version.engine ~= enums.engineVersion then
    assert('Version ' .. chapter.version.engine .. ' of book isn\'t equal to the version ' .. enums.engineVersion .. ' of Narrator.')
  end

  -- Merge the root knot and it's stitch
  book.tree._._ = lume.concat(chapter.tree._._, book.tree._._)
  chapter.tree._._ = nil
  book.tree._ = lume.merge(chapter.tree._, book.tree._)
  chapter.tree._ = nil

  -- Merge a chapter to a book
  book.tree = lume.merge(book.tree or { }, chapter.tree or { })
  book.constants = lume.merge(book.constants or { }, chapter.constants or { })
  book.lists = lume.merge(book.lists or { }, chapter.lists or { })
  book.variables = lume.merge(book.variables or { }, chapter.variables or { })
  
  return book
end

--
-- Public

local Narrator = { }

--- Parse a book from an Ink file
-- Use parsing in development, but prefer already parsed and stored books in production
-- Required: lpeg, io
-- @param path string: path to an Ink file
-- @param params table: parameters { save }
-- @param params.save boolean: save a parsed book to a lua file
-- @return a book
function Narrator.parseFile(path, params)
  local params = params or { save = false }
  assert(parser, "Can't parse anything without lpeg, sorry.")

  local content = readFile(path)
  local book = parser.parse(content)
  
  for _, inclusion in ipairs(book.inclusions) do
    local folderPath = path:match('(.*' .. folderSeparator .. ')')
    local inclusionPath = folderPath .. clearPath(inclusion) .. '.ink'
    local chapter = Narrator.parseFile(inclusionPath)
    merge(book, chapter)
  end  

  if params.save then
    saveBook(book, path)
  end

  return book
end

--- Parse a book from Ink content
-- Use parsing in development, but prefer already parsed and stored books in production
-- Required: lpeg
-- @param content string: root Ink content
-- @param inclusions table: an array of strings with Ink content inclusions
-- @return table: a book
function Narrator.parseBook(content, inclusions)
  local inclusions = inclusions or { }
  assert(parser, "Can't parse anything without a parser.")
  
  local book = parser.parse(content)
  
  for _, inclusion in ipairs(inclusions) do
    local chapter = parser.parse(inclusion)
    merge(book, chapter)
  end  

  return book
end

--- Init a story from a book
-- @param book table: a book
-- @return table: a story
function Narrator.initStory(book)
  local story = Story(book)
  return story
end

return Narrator
end)
__bundle_register("narrator.story", function(require, _LOADED, __bundle_register, __bundle_modules)
--
-- Dependencies

local enums = require("narrator.enums")
local lume = require("narrator.libs.lume")
local Object = require("narrator.libs.classic")
local listMT = require("narrator.list.mt")

--
-- Story

local Story = Object:extend()

--
-- Initialization

function Story:new(book)
  self.tree = book.tree
  self.constants = book.constants
  self.variables = lume.clone(book.variables)
  self.lists = book.lists
  
  self.listMT = listMT
  self.listMT.lists = self.lists

  self.version = book.constants.version or 0
  self.migrate = function(state, oldVersion, newVersion) return state end

  self.functions = self:inkFunctions()
  self.observers = { }
  self.globalTags = self:getTags()

  self.temp = { }
  self.seeds = { }
  self.choices = { }
  self.paragraphs = { }
  self.output = { }
  self.visits = { }
  self.currentPath = nil
  self.isOver = false
end

--
-- Public

--- Start a story.
-- Generate the first chunk of paragraphs and choices.
function Story:begin()
  if #self.paragraphs > 0 or #self.choices > 0 then
    return
  end

  self:jumpTo('_')
end

--- Does the story have paragraphs to output or not.
-- @return boolean: can continue or not
function Story:canContinue()
  return #self.paragraphs > 0
end

--- Get all the current paragraphs or pull them step by step.
-- @param steps number: count of paragraphs to get. 
-- @return table: an array of paragraphs
function Story:continue(steps)
  local lines = { }

  if not self:canContinue() then
    return lines
  end

  local steps = steps or 0
  local singleMode = steps == 1

  steps = steps > 0 and steps or #self.paragraphs
  steps = steps > #self.paragraphs and #self.paragraphs or steps

  for index = 1, steps do
    local paragraph = self.paragraphs[index]
    paragraph.text = paragraph.text:gsub('^%s*(.-)%s*$', '%1')
    table.insert(lines, paragraph)
    table.insert(self.output, paragraph)
  end
  for index = 1, steps do
    table.remove(self.paragraphs, 1)
  end

  return singleMode and lines[1] or lines
end

--- Does the story have choices to output or not.
-- Also returns false if there are available paragraphs to continue.
-- @return boolean: has choices or not
function Story:canChoose()
  return self.choices ~= nil and #self.choices > 0 and not self:canContinue()
end

--- Returns an array of available choice titles. 
-- Also returns an empty array if there are available paragraphs to continue.
-- @return table: an array of choice titles
function Story:getChoices()
  local choices = { }

  if self:canContinue() then
    return choices
  end
  
  for _, choice in ipairs(self.choices) do
    local model = {
      text = choice.title,
      tags = choice.tags
    }
    table.insert(choices, model)
  end

  return choices
end

--- Make a choice to continue the story.
-- @param index number: an index of the choice
function Story:choose(index)
  if self:canContinue() then
    return
  end
  
  local choiceIsAvailable = index > 0 and index <= #self.choices
  assert(choiceIsAvailable, 'Choice index ' .. index .. ' out of bounds 1-' .. #self.choices)

  local choice = self.choices[index]
  assert(choice, 'Choice index ' .. index .. ' out of bounds 1-' .. #self.choices)
  
  self.paragraphs = { }
  self.choices = { }

  if choice.text and #choice.text > 0 then
    local paragraph = {
      text = choice.text,
      tags = choice.tags
    }
    table.insert(self.paragraphs, paragraph)
  end

  self:visit(choice.path)
  if choice.divert ~= nil then
    self:jumpTo(choice.divert)
  else
    self:readPath(choice.path)
  end
end

--- Jumps to the path
-- @param pathString string: a path string like 'knot.stitch.label'
function Story:jumpTo(pathString)
  assert(pathString, 'The pathString can\'t be nil')

  self.choices = { }

  if pathString == 'END' or pathString == 'DONE' then
    self.isOver = true
    return
  end

  local path = self:pathFromString(pathString, self.currentPath)

  if path.label ~= nil then
    path.chain = self:pathChainForLabel(path)
  end

  self:readPath(path)
end

--- Returns the number of visits to the path.
-- @param pathString string: a path string like 'knot.stitch.label'
function Story:getVisits(pathString, context)
  local path = self:pathFromString(pathString, context)
  local visitsCount = self:getVisitsForPath(path)
  return visitsCount
end

--- Get tags for the path
-- @param pathString string: a path string with knot or stitch
-- @return table: an array of tags
function Story:getTags(pathString)
  local path = self:pathFromString(pathString)
  local items = self:itemsFor(path.knot, path.stitch)
  local tags = { }

  for _, item in ipairs(items) do
    if type(item) == 'table' and lume.count(item) > 1 or item.tags == nil then break end
    local itemTags = type(item.tags) == 'string' and { item.tags } or item.tags
    tags = lume.concat(tags, itemTags)
  end

  return tags
end

--- Returns a table with the story state that can be saved and restored later.
-- Use it to save the game.
-- @return table: a story's state
function Story:saveState()
  local state = {
    version = self.version,
    temp = self.temp,
    seeds = self.seeds,
    variables = self.variables,
    visits = self.visits,
    path = self.currentPath,
    paragraphs = self.paragraphs,
    choices = self.choices,
    output = self.output
  }
  return state
end

--- Restores a story's state from the saved before state.
-- Use it to load the game.
-- @param state table: a saved before state
function Story:loadState(state)
  if self.version ~= state.version then
    state = self.migrate(state, state.version, self.version)
  end

  self.temp = state.temp
  self.seeds = state.seeds
  self.variables = state.variables
  self.visits = state.visits
  self.currentPath = state.path
  self.paragraphs = state.paragraphs
  self.choices = state.choices
  self.output = state.output
end

--- Assigns an observer function to the variable's changes.
-- @param variable string: a name of the Ink variable
-- @param observer function: an observer function
function Story:observe(variable, observer)
  self.observers[variable] = observer
end


--- Binds a function to external calling from the Ink.
-- The function can returns the value or not.
-- @param funcName string: a name of the function used in the Ink content
-- @param handler function: a name of the function used in the Ink content
function Story:bind(funcName, handler)
  self.functions[funcName] = handler
end


--
-- Private

function Story:pathChainForLabel(path)
  local label = path.label
  local items = self:itemsFor(path.knot, path.stitch)

  -- TODO: Find a more smart solution to divert to labels
  -- TODO: This works but... isn't good.

  local function findLabelChainInItems(items)
    for index, item in ipairs(items) do

      if item.label == label then return { index }

      elseif item.node ~= nil then
        local result = findLabelChainInItems(item.node)
        if result ~= nil then
          table.insert(result, 0, index)
          return result
        end

      elseif item.success ~= nil then
        if type(item.success) == 'table' then
          local isSwitch = item.success[1] ~= nil and item.success[1][1] ~= nil
          local cases = isSwitch and item.success or { item.success }
          for caseIndex, case in ipairs(cases) do
            local result = findLabelChainInItems(case)
            if result ~= nil then
              table.insert(result, 0, 't' .. caseIndex)
              table.insert(result, 0, index)
              return result
            end
          end
        end

        if type(item.failure) == 'table' then
          local result = findLabelChainInItems(item.failure)
          if result ~= nil then
            table.insert(result, 0, 'f')
            table.insert(result, 0, index)
            return result
          end
        end
      end
    end

    return nil
  end

  local chain = findLabelChainInItems(items)
  assert(chain, 'Label \'' ..path.label .. '\' not found')
  return chain
end

function Story:readPath(path)
  assert(path, 'The reading path can\'t be nil')

  if self.isOver then
    return
  end

  -- Visit only the paths without labels.
  -- Items with labels will increment visits counter by themself in readItems().
  if not path.label then
    self:visit(path)
  end

  local items = self:itemsFor(path.knot, path.stitch)
  self:readItems(items, path)
end

function Story:itemsFor(knot, stitch)
  local rootNode = self.tree
  local knotNode = knot == nil and rootNode._ or rootNode[knot]
  assert(knotNode or lume.isarray(rootNode), 'The knot \'' .. (knot or '_') .. '\' not found')
  local stitchNode = stitch == nil and knotNode._ or knotNode[stitch]
  assert(stitchNode or lume.isarray(knotNode), 'The stitch \'' .. (knot or '_') .. '.' .. (stitch or '_') .. '\' not found')
  return stitchNode or knotNode or rootNode
end

function Story:readItems(items, path, depth, mode)
  assert(items, 'Items can\'t be nil')
  assert(path, 'Path can\'t be nil')

  local chain = path.chain or { }
  local depth = depth or 0
  local deepIndex = chain[depth + 1]
  local mode = mode or enums.readMode.text

  -- Deep path factory

  local makeDeepPath = function(values, labelPrefix)
    local deepChain = lume.slice(chain, 1, depth)
    for valuesIndex, value in ipairs(values) do
      deepChain[depth + valuesIndex] = value
    end
    local deepPath = lume.clone(path)
    deepPath.chain = deepChain
    if labelPrefix then
      deepPath.label = labelPrefix .. table.concat(deepChain, '.')
    end
    return deepPath
  end

  -- Iterate items

  for index = deepIndex or 1, #items do
    local item = items[index]
    local skip = false

    local itemType = enums.item.text
    if type(item) == 'table' then
      if item.choice ~= nil then itemType = enums.item.choice
      elseif item.success ~= nil then itemType = enums.item.condition
      elseif item.var ~= nil then itemType = enums.item.variable
      elseif item.alts ~= nil then itemType = enums.item.alts
      end
    end

    -- Go deep
    if index == deepIndex then
      if itemType == enums.item.choice and item.node ~= nil then
        -- Go deep to the choice node
        mode = enums.readMode.gathers
        mode = self:readItems(item.node, path, depth + 1) or mode
        
      elseif itemType == enums.item.condition then
        -- Go deep to the condition node
        local chainValue = chain[depth + 2]
        local isSuccess = chainValue:sub(1, 1) == 't'

        local node
        if isSuccess then
          local successIndex = tonumber(chainValue:sub(2, 2)) or 0
          node = successIndex > 0 and item.success[successIndex] or item.success
        else
          node = item.failure
        end

        mode = self:readItems(node, path, depth + 2, mode) or mode
      end

      if itemType == enums.item.condition or itemType == enums.item.choice then
        mode = mode ~= enums.readMode.quit and enums.readMode.gathers or mode
        skip = true
      end
    end

    -- Check the situation
    if mode == enums.readMode.choices and itemType ~= enums.item.choice then
      mode = enums.readMode.quit
      skip = true
    elseif mode == enums.readMode.gathers and itemType == enums.item.choice then
      skip = true
    end
    
    -- Read the item
    if skip then
      -- skip
    elseif itemType == enums.item.text then
      mode = enums.readMode.text
      local safeItem = type(item) == 'string' and { text = item } or item
      mode = self:readText(safeItem) or mode
    elseif itemType == enums.item.alts then
      mode = enums.readMode.text
      local deepPath = makeDeepPath({ index }, '~')
      mode = self:readAlts(item, deepPath, depth + 1, mode) or mode
    elseif itemType == enums.item.choice and self:checkCondition(item.condition) then
      mode = enums.readMode.choices
      local deepPath = makeDeepPath({ index }, '>')
      deepPath.label = item.label or deepPath.label
      mode = self:readChoice(item, deepPath) or mode
      if index == #items and type(chain[#chain]) == 'number' then
        mode = enums.readMode.quit
      end
    elseif itemType == enums.item.condition then
      local result, chainValue
      if type(item.condition) == 'string' then  
        local success = self:checkCondition(item.condition)
        result = success and item.success or (item.failure or { })
        chainValue = success and 't' or 'f'
      elseif type(item.condition) == 'table' then
        local success = self:checkSwitch(item.condition)
        result = success > 0 and item.success[success] or (item.failure or { })
        chainValue = success > 0 and ('t' .. success) or 'f'
      end
      if type(result) == 'string' then
        mode = enums.readMode.text
        mode = self:readText({ text = result }) or mode
      elseif type(result) == 'table' then
        local deepPath = makeDeepPath({ index, chainValue })
        mode = self:readItems(result, deepPath, depth + 2, mode) or mode
      end
    elseif itemType == enums.item.variable then
      self:assignValueTo(item.var, item.value, item.temp)
    end

    -- Read the label
    if item.label ~= nil and itemType ~= enums.item.choice and not skip then
      local labelPath = lume.clone(path)
      labelPath.label = item.label
      self:visit(labelPath)
    end

    if mode == enums.readMode.quit then
      break
    end
  end

  if depth == 0 then
    for index = #self.paragraphs, 1, -1 do
      local paragraph = self.paragraphs[index]
      if (not paragraph.text or #paragraph.text == 0) and (not paragraph.tags or #paragraph.tags == 0) then
        -- Remove safe prefixes and suffixes of failured inline conditions
        table.remove(self.paragraphs, index)
      else
        -- Remove <> tail from unexpectedly broken paragraphs
        paragraph.text = paragraph.text:match('(.-)%s*<>$') or paragraph.text
      end
    end
  end

  return mode
end

function Story:readText(item)
  local text = item.text
  local tags = type(item.tags) == 'string' and { item.tags } or item.tags

  if text ~= nil or tags ~= nil then
    local paragraph = { text = text or '<>', tags = tags }
    local gluedByPrev = #self.paragraphs > 0 and self.paragraphs[#self.paragraphs].text:sub(-2) == '<>' 
    local gluedByThis = text ~= nil and text:sub(1, 2) == '<>'
    
    paragraph.text = self:replaceExpressions(paragraph.text)
    paragraph.text = paragraph.text:gsub('%s+', ' ')

    if gluedByPrev then
      local prevParagraph = self.paragraphs[#self.paragraphs]
      prevParagraph.text = prevParagraph.text:sub(1, #prevParagraph.text - 2)
      self.paragraphs[#self.paragraphs] = prevParagraph
    end

    if gluedByThis then
      paragraph.text = paragraph.text:sub(3)
    end

    if gluedByPrev or (gluedByThis and #self.paragraphs > 0) then
      local prevParagraph = self.paragraphs[#self.paragraphs]
      prevParagraph.text = (prevParagraph.text .. paragraph.text):gsub('%s+', ' ')
      prevParagraph.tags = lume.concat(prevParagraph.tags, paragraph.tags)
      prevParagraph.tags = #prevParagraph.tags > 0 and prevParagraph.tags or nil
      self.paragraphs[#self.paragraphs] = prevParagraph
    else
      table.insert(self.paragraphs, #self.paragraphs + 1, paragraph)
    end
  end

  if item.divert ~= nil then
    self:jumpTo(item.divert)
    return enums.readMode.quit
  end
end

function Story:readAlts(item, path, depth, mode)
  assert(item.alts, 'Alternatives can\'t be nil')
  local alts = lume.clone(item.alts)

  local sequence = item.sequence or enums.sequence.stopping
  if type(sequence) == 'string' then
    sequence = enums.sequence[item.sequence] or sequence
  end

  self:visit(path)
  local visits = self:getVisitsForPath(path)
  local index = 0

  if item.shuffle then
    local seedKey = (path.knot or '_') .. '.' .. (path.stitch or '_') .. ':' .. path.label
    local seed = visits % #alts == 1 and (self.debugSeed or os.time() * 1000) or self.seeds[seedKey]
    self.seeds[seedKey] = seed

    for index, alt in ipairs(alts) do
      math.randomseed(seed + index)

      local pairIndex = index < #alts and math.random(index, #alts) or index
      alts[index] = alts[pairIndex]
      alts[pairIndex] = alt
    end
  end

  if sequence == enums.sequence.cycle then
    index = visits % #alts
    index = index > 0 and index or #alts
  elseif sequence == enums.sequence.stopping then
    index = visits < #alts and visits or #alts
  elseif sequence == enums.sequence.once then
    index = visits
  end

  local alt = index <= #alts and alts[index] or { }
  local items = type(alt) == 'string' and { alt } or alt
  return self:readItems(items, path, depth, mode)
end

function Story:readChoice(item, path)
  local isFallback = item.choice == 0

  if isFallback then
    -- Works correctly only when a fallback is the last choice
    if #self.choices == 0 then
      if item.divert ~= nil then
        self:jumpTo(item.divert)
      else
        self:readPath(path)
      end
    end
    return enums.readMode.quit
  end

  local title = self:replaceExpressions(item.choice)
  title = title:match('(.-)%s*<>$') or title

  local choice = {
    title = title,
    text = item.text ~= nil and self:replaceExpressions(item.text) or title,
    divert = item.divert,
    tags = item.tags,
    path = path
  }

  if item.sticky or self:getVisitsForPath(path) == 0 then
    table.insert(self.choices, #self.choices + 1, choice)
  end
end


-- Expressions

function Story:replaceExpressions(text)
  return text:gsub('%b##', function(match)
    if #match == 2 then
      return '#'
    else
      local result = self:doExpression(match:sub(2, #match - 1))

      if type(result) == 'table' then
        result = self.listMT.__tostring(result)
      elseif type(result) == 'boolean' then
        result = result and 1 or 0
      elseif type(result) == 'number' then
        result = tostring(result)
        if result:sub(-2) == '.0' then
          result = result:sub(1, -3)
        end
      elseif result == nil then
        result = ''
      end

      return result
    end
  end)
end

function Story:checkSwitch(conditions)
  for index, condition in ipairs(conditions) do
    if self:checkCondition(condition) then
      return index
    end
  end
  return 0
end

function Story:checkCondition(condition)
  if condition == nil then
    return true
  end

  local result = self:doExpression(condition)

  if type(result) == 'table' and not next(result) then
    result = nil
  end
  
  return result ~= nil and result ~= false
end

function Story:doExpression(expression)
  assert(type(expression) == 'string', 'Expression must be a string')

  local code = ''
  local lists = { }
  
  -- Replace operators
  expression = expression:gsub('!=', '~=')
  expression = expression:gsub('%s*||%s*', ' or ')  
  expression = expression:gsub('%s*%&%&%s*', ' and ')
  expression = expression:gsub('%s+has%s+', ' ? ')
  expression = expression:gsub('%s+hasnt%s+', ' !? ')
  
  -- Replace functions results
  expression = expression:gsub('[%a_][%w_]*%b()', function(match)
    local functionName = match:match('([%a_][%w_]*)%(')
    local paramsString = match:match('[%a_][%w_]*%((.+)%)')
    local params = paramsString ~= nil and lume.map(lume.split(paramsString, ','), lume.trim) or nil

    for index, param in ipairs(params or { }) do
      params[index] = self:doExpression(param)
    end

    local func = self.functions[functionName]
    if func ~= nil then
      local value = func((table.unpack or unpack)(params or { }))
      if type(value) == 'table' then
        lists[#lists + 1] = value
        return '__list' .. #lists
      else
        return lume.serialize(value)
      end
    elseif self.lists[functionName] ~= nil then
      local index = params and params[1] or 0
      local item = self.lists[functionName][index]
      local list = item and { [functionName] = { [item] = true } } or { }
      lists[#lists + 1] = list
      return '__list' .. #lists
    end
    
    return 'nil'
  end)

  -- Replace lists
  expression = expression:gsub('%(([%s%w%.,_]*)%)', function(match)
    local list = self:makeListFor(match)
    if list ~= nil then
      lists[#lists + 1] = list
      return '__list' .. #lists
    else
      return 'nil'
    end
  end)
  
  -- Store strings to the bag before to replace variables
  -- otherwise it can replace strings inside quotes to nils.
  -- Info: Ink doesn't interpret single quotes '' as string expression value
  local stringsBag = { }
  expression = expression:gsub('%b\"\"', function(match)
    table.insert(stringsBag, match)
    return '#' .. #stringsBag .. '#'
  end)

  -- Replace variables
  expression = expression:gsub('[%a_][%w_%.]*', function(match)
    local exceptions = { 'and', 'or', 'true', 'false', 'nil', 'not'}
    if lume.find(exceptions, match) or match:match('__list%d*') then
      return match
    else
      local value = self:getValueFor(match)
      if type(value) == 'table' then
        lists[#lists + 1] = value
        return '__list' .. #lists
      else
        return lume.serialize(value)
      end
    end
  end)

  -- Replace with math results
  expression = expression:gsub('[%a_#][%w_%.#]*[%s]*[%?!]+[%s]*[%a_#][%w_%.#]*', function(match)
    local lhs, operator, rhs = match:match('([%a_#][%w_%.#]*)[%s]*([%!?]+)[%s]*([%a_#][%w_%.#]*)')
    if lhs:match('__list%d*') then
      return lhs .. ' % ' .. rhs .. (operator == '?' and ' == true' or ' == false')
    else
      return 'string.match(' .. lhs .. ', ' .. rhs .. ')' .. (operator == '?' and ' ~= nil' or ' == nil')
    end
  end)

  -- Restore strings after variables replacement
  expression = expression:gsub('%b##', function(match)
    local index = tonumber(match:sub(2, -2))
    return stringsBag[index or 0]
  end)

  -- Attach the metatable to list tables
  if #lists > 0 then
    code = code .. 'local mt = require(\'narrator.list.mt\')\n'
    code = code .. 'mt.lists = ' .. lume.serialize(self.lists) .. '\n\n'
    for index, list in pairs(lists) do
      local name = '__list' .. index
      code = code .. 'local ' .. name .. ' = ' .. lume.serialize(list) .. '\n'
      code = code .. 'setmetatable(' .. name .. ', mt)\n\n'
    end
  end
  
  code = code .. 'return ' .. expression
  return lume.dostring(code)
end


-- Variables

function Story:assignValueTo(variable, expression, temp)
  if self.constants[variable] ~= nil then
    return
  end
  local value = self:doExpression(expression)

  if #variable == 0 then
    return
  end
  local storage = (temp or self.temp[variable] ~= nil) and self.temp or self.variables
  
  if storage[variable] == value then
    return
  end
  storage[variable] = value

  local observer = self.observers[variable]
  if observer ~= nil then
    observer(value)
  end
end

function Story:getValueFor(variable)
  local result = self.temp[variable]
  
  if result == nil then
    result = self.variables[variable]
  end
  if result == nil then
    result = self.constants[variable]
  end
  if result == nil then
    result = self:makeListFor(variable)
  end
  if result == nil then
    local visits = self:getVisits(variable, self.currentPath)
    result = visits > 0 and visits or nil
  end

  return result
end


-- Lists

function Story:makeListFor(expression)
  local result = { }
  if not expression:find('%S') then
    return result
  end

  local items = lume.array(expression:gmatch('[%w_%.]+'))
  
  for _, item in ipairs(items) do
    local listName, itemName = self:getListNameFor(item)
    if listName ~= nil and itemName ~= nil then 
      result[listName] = result[listName] or { }
      result[listName][itemName] = true
    end
  end

  return next(result) ~= nil and result or nil
end

function Story:getListNameFor(name)
  local listName, itemName = name:match('([%w_]+)%.([%w_]+)')
  itemName = itemName or name

  if listName == nil then
    for key, list in pairs(self.lists) do
      for index, string in ipairs(list) do
        if string == itemName then
          listName = key
          break
        end
      end
    end
  end

  local notFound = listName == nil or self.lists[listName] == nil
  if notFound then return nil end
  return listName, itemName
end


-- Visits

function Story:visit(path)
  local pathIsChanged = self.currentPath == nil or path.knot ~= self.currentPath.knot or path.stitch ~= self.currentPath.stitch

  if pathIsChanged then
    if self.currentPath == nil or path.knot ~= self.currentPath.knot then
      local knot = path.knot or '_'
      local visits = self.visits[knot] or { _root = 0 }
      visits._root = visits._root + 1
      self.visits[knot] = visits
    end
  
    local knot, stitch = path.knot or '_', path.stitch or '_'
    local visits = self.visits[knot][stitch] or { _root = 0 }
    visits._root = visits._root + 1
    self.visits[knot][stitch] = visits
  end

  if path.label ~= nil then
    local knot, stitch, label = path.knot or '_', path.stitch or '_', path.label
    self.visits[knot] = self.visits[knot] or { _root = 1, _ = { _root = 1 } } 
    self.visits[knot][stitch] = self.visits[knot][stitch] or { _root = 1 }
    local visits = self.visits[knot][stitch][label] or 0
    visits = visits + 1
    self.visits[knot][stitch][path.label] = visits
  end

  self.currentPath = lume.clone(path)
  self.currentPath.label = nil
  self.temp = pathIsChanged and { } or self.temp
end

function Story:getVisitsForPath(path)
  if path == nil then return 0 end
  local knot, stitch, label = path.knot or '_', path.stitch, path.label
  if stitch == nil and label ~= nil then stitch = '_' end

  local knotVisits = self.visits[knot]
  if knotVisits == nil then return 0
  elseif stitch == nil then return knotVisits._root or 0 end

  local stitchVisits = knotVisits[stitch]
  if stitchVisits == nil then return 0
  elseif label == nil then return stitchVisits._root or 0 end

  local labelVisits = stitchVisits[label]
  return labelVisits or 0
end

function Story:pathFromString(pathString, context)
  local pathString = pathString or ''
  local contextKnot = context and context.knot
  local contextStitch = context and context.stitch
  
  contextKnot = contextKnot or '_'
  contextStitch = contextStitch or '_'

  -- Try to parse 'part1.part2.part3'
  local part1, part2, part3 = pathString:match('([%w_]+)%.([%w_]+)%.([%w_]+)')
  
  if not part1 then
    -- Try to parse 'part1.part2'
    part1, part2 = pathString:match('([%w_]+)%.([%w_]+)')
  end

  if not part1 then
    -- Try to parse 'part1'
    part1 = #pathString > 0 and pathString or nil
  end

  local path = { }
  
  if not part1 then
    -- Path is empty
    return path
  end

  if part3 then
    -- Path is 'part1.part2.part3'
    path.knot = part1
    path.stitch = part2
    path.label = part3
    return path
  end
  
  if part2 then
    -- Path is 'part1.part2'

    if self.tree[part1] and self.tree[part1][part2] then
      -- Knot 'part1' and stitch 'part2' exist so return part1.part2
      path.knot = part1
      path.stitch = part2
      return path
    end
    
    if self.tree[contextKnot][part1] then
      -- Stitch 'part1' exists so return contextKnot.part1.part2
      path.knot = contextKnot
      path.stitch = part1
      path.label = part2
      return path
    end
    
    if self.tree[part1] then
      -- Knot 'part1' exists so seems it's a label with a root stitch
      path.knot = part1
      path.stitch = '_'     
      path.label = part2
      return path
    end

    if self.tree._[part1] then
      -- Root stitch 'part1' exists so return _.part1.part2
      path.knot = '_'
      path.stitch = part1
      path.label = part2
      return path
    end
  end
  
  if part1 then
    -- Path is 'part1'
    if self.tree[contextKnot][part1] then
      -- Stitch 'part1' exists so return contextKnot.part1
      path.knot = contextKnot
      path.stitch = part1
      return path
    elseif self.tree[part1] then
      -- Knot 'part1' exists so return part1
      path.knot = part1
      return path
    else
      -- Seems it's a label
      path.knot = contextKnot
      path.stitch = contextStitch
      path.label = part1
    end    
  end

  return path
end


-- Ink functions

function Story:inkFunctions()
  return {
    CHOICE_COUNT = function() return #self.choices end,
    SEED_RANDOM = function(seed) self.debugSeed = seed end,
    POW = function(x, y) return math.pow(x, y) end,
    RANDOM = function(x, y)
      math.randomseed(self.debugSeed or os.clock() * 1000)
      return math.random(x, y)
    end,
    INT = function(x) return math.floor(x) end,
    FLOOR = function(x) return math.floor(x) end,
    FLOAT = function(x) return x end,

    -- TURNS = function() return nil end -- TODO
    -- TURNS_SINCE = function(path) return nil end -- TODO  

    LIST_VALUE = function(list) return self.listMT.firstRawValueOf(list) end,
    LIST_COUNT = function(list) return self.listMT.__len(list) end,
    LIST_MIN = function(list) return self.listMT.minValueOf(list) end,
    LIST_MAX = function(list) return self.listMT.maxValueOf(list) end,
    LIST_RANDOM = function(list)
      math.randomseed(self.debugSeed or os.clock() * 1000)
      return self.listMT.randomValueOf(list)
    end,
    LIST_ALL = function(list) return self.listMT.posibleValuesOf(list) end,
    LIST_RANGE = function(list, min, max) return self.listMT.rangeOf(list, min, max) end,
    LIST_INVERT = function(list) return self.listMT.invert(list) end
  }
end

return Story
end)
__bundle_register("narrator.list.mt", function(require, _LOADED, __bundle_register, __bundle_modules)
--
-- Dependencies

local lume = require("narrator.libs.lume")

--
-- Metatable

local mt = { lists = { } }

function mt.__tostring(self)
  local pool = { }

  local listKeys = { }
  for key, _ in pairs(self) do
    table.insert(listKeys, key)
  end
  table.sort(listKeys)

  for i = 1, #listKeys do
    local listName = listKeys[i]
    local listItems = self[listName]
    for index = 1, #mt.lists[listName] do
      pool[index] = pool[index] or { }
      local itemName = mt.lists[listName][index]
      if listItems[itemName] == true then
        table.insert(pool[index], 1, itemName)
      end
    end
  end

  local items = { }

  for _, titles in ipairs(pool) do
    for _, title in ipairs(titles) do
      table.insert(items, title)
    end
  end

  return table.concat(items, ', ')
end

--
-- Operators

function mt.__add(lhs, rhs) -- +
  if type(rhs) == 'table' then
    return mt.__addList(lhs, rhs)
  elseif type(rhs) == 'number' then
    return mt.__shiftByNumber(lhs, rhs)
  else
    error('Attempt to sum the list with ' .. type(rhs))
  end
end

function mt.__sub(lhs, rhs) -- -
  if type(rhs) == 'table' then
    return mt.__subList(lhs, rhs)
  elseif type(rhs) == 'number' then
    return mt.__shiftByNumber(lhs, -rhs)
  else
    error('Attempt to sub the list with ' .. type(rhs))
  end
end

function mt.__mod(lhs, rhs) -- % (contain)
  if type(rhs) ~= 'table' then
    error('Attempt to check content of the list for ' .. type(rhs))
  end

  for listName, listItems in pairs(rhs) do
    if lhs[listName] == nil then return false end
    for itemName, itemValue in pairs(listItems) do
      if (lhs[listName][itemName] or false) ~= itemValue then return false end
    end  
  end

  return true
end

function mt.__pow(lhs, rhs) -- ^ (intersection)
  if type(rhs) ~= 'table' then
    error('Attempt to interselect the list with ' .. type(rhs))
  end

  local intersection = { }
  
  for listName, listItems in pairs(lhs) do
    for itemName, itemValue in pairs(listItems) do
      local left = lhs[listName][itemName]
      local right = (rhs[listName] or { })[itemName]
      if left == true and right == true then
        intersection[listName] = intersection[listName] or { }
        intersection[listName][itemName] = true
      end
    end
  end

  setmetatable(intersection, mt)
  return intersection
end

function mt.__len(self) -- #
  local len = 0

  for listName, listItems in pairs(self) do
    for itemName, itemValue in pairs(listItems) do
      if itemValue == true then len = len + 1 end
    end
  end

  return len
end

function mt.__eq(lhs, rhs) -- ==
  if type(rhs) ~= 'table' then
    error('Attempt to compare the list with ' .. type(rhs))
  end

  local function keysCount(object) 
    local count = 0
    for _, _ in pairs(object) do
      count = count + 1
    end  
    return count
  end

  local leftListsCount = keysCount(lhs)
  local rightListsCount = keysCount(rhs)
  if leftListsCount ~= rightListsCount then
    return false
  end

  for listName, leftItems in pairs(lhs) do
    local rightItems = rhs[listName]
    if rightItems == nil then
      return false
    end

    local leftItemsCount = keysCount(leftItems)
    local rightItemsCount = keysCount(rightItems)
  
    if leftItemsCount ~= rightItemsCount then
      return false
    end
  end

  return mt.__mod(lhs, rhs)
end

function mt.__lt(lhs, rhs) -- <
  if type(rhs) ~= 'table' then
    error('Attempt to compare the list with ' .. type(rhs))
  end

  -- LEFT < RIGHT means "the smallest value in RIGHT is bigger than the largest values in LEFT"
  
  local minLeft = mt.minValueOf(lhs, true)
  local maxRight = mt.maxValueOf(rhs, true)

  return minLeft < maxRight
end

function mt.__le(lhs, rhs) -- <=
  if type(rhs) ~= 'table' then
    error('Attempt to compare the list with ' .. type(rhs))
  end

  -- LEFT => RIGHT means "the smallest value in RIGHT is at least the smallest value in LEFT,
  --                  and the largest value in RIGHT is at least the largest value in LEFT".

  local minRight = mt.minValueOf(rhs, true)
  local minLeft = mt.minValueOf(lhs, true)
  local maxRight = mt.maxValueOf(rhs, true)
  local maxLeft = mt.maxValueOf(lhs, true)

  return minRight >= minLeft and maxRight >= maxLeft
end

--
-- Custom operators

function mt.__addList(lhs, rhs)
  local result = lume.clone(lhs)

  for listName, listItems in pairs(rhs) do
    result[listName] = result[listName] or { }
    for itemName, itemValue in pairs(listItems) do
      result[listName][itemName] = itemValue
    end
  end

  return result
end

function mt.__subList(lhs, rhs)
  local result = lume.clone(lhs)

  for listName, listItems in pairs(rhs) do
    if lhs[listName] ~= nil then
      for itemName, _ in pairs(listItems) do
        lhs[listName][itemName] = nil
      end  
    end
  end

  return mt.removeEmptiesInList(result)
end

function mt.__shiftByNumber(list, number)
  local result = { }

  for listName, listItems in pairs(list) do
    result[listName] = { }
    for index, itemName in ipairs(mt.lists[listName]) do
      if listItems[itemName] == true then
        local nextItem = mt.lists[listName][index + number]
        if nextItem ~= nil then
          result[listName][nextItem] = true
        end
      end
    end
  end

  return mt.removeEmptiesInList(result)
end

--
-- Helpers

function mt.removeEmptiesInList(list)
  local result = lume.clone(list)

  for listName, listItems in pairs(list) do
    if next(listItems) == nil then
      result[listName] = nil
    end
  end

  return result
end

function mt.minValueOf(list, raw)
  local minIndex = 0
  local minValue = { }

  local listKeys = { }
  for key, _ in pairs(list) do
    table.insert(listKeys, key)
  end
  table.sort(listKeys)

  for i = 1, #listKeys do
    local listName = listKeys[i]
    local listItems = list[listName]
    for itemName, itemValue in pairs(listItems) do
      if itemValue == true then
        local index = lume.find(mt.lists[listName], itemName)
        if index and index < minIndex or minIndex == 0 then
          minIndex = index
          minValue = { [listName] = { [itemName] = true } }
        end
      end
    end
  end

  return raw and minIndex or minValue
end

function mt.maxValueOf(list, raw)
  local maxIndex = 0
  local maxValue = { }

  local listKeys = { }
  for key, _ in pairs(list) do
    table.insert(listKeys, key)
  end
  table.sort(listKeys)

  for i = 1, #listKeys do
    local listName = listKeys[i]
    local listItems = list[listName]
    for itemName, itemValue in pairs(listItems) do
      if itemValue == true then
        local index = lume.find(mt.lists[listName], itemName)
        if index and index > maxIndex or maxIndex == 0 then
          maxIndex = index
          maxValue = { [listName] = { [itemName] = true } }
        end
      end
    end
  end

  return raw and maxIndex or maxValue
end

function mt.randomValueOf(list)
  local items = { }

  local listKeys = { }
  for key, _ in pairs(list) do
    table.insert(listKeys, key)
  end
  table.sort(listKeys)

  for i = 1, #listKeys do
    local listName = listKeys[i]
    local listItems = list[listName]
    local itemsKeys = { }
    for key, _ in pairs(listItems) do
      table.insert(itemsKeys, key)
    end    
    table.sort(itemsKeys)

    for i = 1, #itemsKeys do
      local itemName = itemsKeys[i]
      local itemValue = listItems[itemName]
      if itemValue == true then
        local result = { [listName] = { [itemName] = true } }
        table.insert(items, result)
      end
    end
  end

  local randomIndex = math.random(1, #items)
  return items[randomIndex]
end

function mt.firstRawValueOf(list)
  local result = 0
  
  for listName, listItems in pairs(list) do
    for itemName, itemValue in pairs(listItems) do
      if itemValue == true then
        local index = lume.find(mt.lists[listName], itemName)
        if index then
          result = index
          break
        end
      end
    end
  end

  return result
end

function mt.posibleValuesOf(list)
  local result = { }

  for listName, listItems in pairs(list) do
    local subList = { }
    for _, itemName in ipairs(mt.lists[listName]) do
      subList[itemName] = true
    end
    result[listName] = subList
  end

  return result
end

function mt.rangeOf(list, min, max)
  if type(min) ~= 'table' and type(min) ~= 'number' then
    error('Attempt to get a range with incorrect min value of type ' .. type(min))
  end
  if type(max) ~= 'table' and type(max) ~= 'number' then
    error('Attempt to get a range with incorrect max value of type ' .. type(max))
  end

  local result = { }
  local allList = mt.posibleValuesOf(list)
  local minIndex = type(min) == 'number' and min or mt.firstRawValueOf(min)
  local maxIndex = type(max) == 'number' and max or mt.firstRawValueOf(max)

  for listName, listItems in pairs(allList) do
    for itemName, itemValue in pairs(listItems) do
      local index = lume.find(mt.lists[listName], itemName)
      if index and index >= minIndex and index <= maxIndex and list[listName][itemName] == true then
        result[listName] = result[listName] or { }
        result[listName][itemName] = true
      end
    end
  end

  return result
end

function mt.invert(list)
  local result = mt.posibleValuesOf(list)

  for listName, listItems in pairs(list) do
    for itemName, itemValue in pairs(listItems) do
      if itemValue == true then
        result[listName][itemName] = nil
      end
    end
  end

  return result
end

return mt
end)
__bundle_register("narrator.libs.lume", function(require, _LOADED, __bundle_register, __bundle_modules)
--
-- lume
--
-- Copyright (c) 2020 rxi
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

local lume = { _version = "2.3.0" }

local pairs, ipairs = pairs, ipairs
local type, assert, unpack = type, assert, unpack or table.unpack
local tostring, tonumber = tostring, tonumber
local math_floor = math.floor
local math_ceil = math.ceil
local math_atan2 = math.atan2 or math.atan
local math_sqrt = math.sqrt
local math_abs = math.abs

local noop = function()
end

local identity = function(x)
  return x
end

local patternescape = function(str)
  return str:gsub("[%(%)%.%%%+%-%*%?%[%]%^%$]", "%%%1")
end

local absindex = function(len, i)
  return i < 0 and (len + i + 1) or i
end

local iscallable = function(x)
  if type(x) == "function" then return true end
  local mt = getmetatable(x)
  return mt and mt.__call ~= nil
end

local getiter = function(x)
  if lume.isarray(x) then
    return ipairs
  elseif type(x) == "table" then
    return pairs
  end
  error("expected table", 3)
end

local iteratee = function(x)
  if x == nil then return identity end
  if iscallable(x) then return x end
  if type(x) == "table" then
    return function(z)
      for k, v in pairs(x) do
        if z[k] ~= v then return false end
      end
      return true
    end
  end
  return function(z) return z[x] end
end



function lume.clamp(x, min, max)
  return x < min and min or (x > max and max or x)
end


function lume.round(x, increment)
  if increment then return lume.round(x / increment) * increment end
  return x >= 0 and math_floor(x + .5) or math_ceil(x - .5)
end


function lume.sign(x)
  return x < 0 and -1 or 1
end


function lume.lerp(a, b, amount)
  return a + (b - a) * lume.clamp(amount, 0, 1)
end


function lume.smooth(a, b, amount)
  local t = lume.clamp(amount, 0, 1)
  local m = t * t * (3 - 2 * t)
  return a + (b - a) * m
end


function lume.pingpong(x)
  return 1 - math_abs(1 - x % 2)
end


function lume.distance(x1, y1, x2, y2, squared)
  local dx = x1 - x2
  local dy = y1 - y2
  local s = dx * dx + dy * dy
  return squared and s or math_sqrt(s)
end


function lume.angle(x1, y1, x2, y2)
  return math_atan2(y2 - y1, x2 - x1)
end


function lume.vector(angle, magnitude)
  return math.cos(angle) * magnitude, math.sin(angle) * magnitude
end


function lume.random(a, b)
  if not a then a, b = 0, 1 end
  if not b then b = 0 end
  return a + math.random() * (b - a)
end


function lume.randomchoice(t)
  return t[math.random(#t)]
end


function lume.weightedchoice(t)
  local sum = 0
  for _, v in pairs(t) do
    assert(v >= 0, "weight value less than zero")
    sum = sum + v
  end
  assert(sum ~= 0, "all weights are zero")
  local rnd = lume.random(sum)
  for k, v in pairs(t) do
    if rnd < v then return k end
    rnd = rnd - v
  end
end


function lume.isarray(x)
  return type(x) == "table" and x[1] ~= nil
end


function lume.push(t, ...)
  local n = select("#", ...)
  for i = 1, n do
    t[#t + 1] = select(i, ...)
  end
  return ...
end


function lume.remove(t, x)
  local iter = getiter(t)
  for i, v in iter(t) do
    if v == x then
      if lume.isarray(t) then
        table.remove(t, i)
        break
      else
        t[i] = nil
        break
      end
    end
  end
  return x
end


function lume.clear(t)
  local iter = getiter(t)
  for k in iter(t) do
    t[k] = nil
  end
  return t
end


function lume.extend(t, ...)
  for i = 1, select("#", ...) do
    local x = select(i, ...)
    if x then
      for k, v in pairs(x) do
        t[k] = v
      end
    end
  end
  return t
end


function lume.shuffle(t)
  local rtn = {}
  for i = 1, #t do
    local r = math.random(i)
    if r ~= i then
      rtn[i] = rtn[r]
    end
    rtn[r] = t[i]
  end
  return rtn
end


function lume.sort(t, comp)
  local rtn = lume.clone(t)
  if comp then
    if type(comp) == "string" then
      table.sort(rtn, function(a, b) return a[comp] < b[comp] end)
    else
      table.sort(rtn, comp)
    end
  else
    table.sort(rtn)
  end
  return rtn
end


function lume.array(...)
  local t = {}
  for x in ... do t[#t + 1] = x end
  return t
end


function lume.each(t, fn, ...)
  local iter = getiter(t)
  if type(fn) == "string" then
    for _, v in iter(t) do v[fn](v, ...) end
  else
    for _, v in iter(t) do fn(v, ...) end
  end
  return t
end


function lume.map(t, fn)
  fn = iteratee(fn)
  local iter = getiter(t)
  local rtn = {}
  for k, v in iter(t) do rtn[k] = fn(v) end
  return rtn
end


function lume.all(t, fn)
  fn = iteratee(fn)
  local iter = getiter(t)
  for _, v in iter(t) do
    if not fn(v) then return false end
  end
  return true
end


function lume.any(t, fn)
  fn = iteratee(fn)
  local iter = getiter(t)
  for _, v in iter(t) do
    if fn(v) then return true end
  end
  return false
end


function lume.reduce(t, fn, first)
  local started = first ~= nil
  local acc = first
  local iter = getiter(t)
  for _, v in iter(t) do
    if started then
      acc = fn(acc, v)
    else
      acc = v
      started = true
    end
  end
  assert(started, "reduce of an empty table with no first value")
  return acc
end


function lume.unique(t)
  local rtn = {}
  for k in pairs(lume.invert(t)) do
    rtn[#rtn + 1] = k
  end
  return rtn
end


function lume.filter(t, fn, retainkeys)
  fn = iteratee(fn)
  local iter = getiter(t)
  local rtn = {}
  if retainkeys then
    for k, v in iter(t) do
      if fn(v) then rtn[k] = v end
    end
  else
    for _, v in iter(t) do
      if fn(v) then rtn[#rtn + 1] = v end
    end
  end
  return rtn
end


function lume.reject(t, fn, retainkeys)
  fn = iteratee(fn)
  local iter = getiter(t)
  local rtn = {}
  if retainkeys then
    for k, v in iter(t) do
      if not fn(v) then rtn[k] = v end
    end
  else
    for _, v in iter(t) do
      if not fn(v) then rtn[#rtn + 1] = v end
    end
  end
  return rtn
end


function lume.merge(...)
  local rtn = {}
  for i = 1, select("#", ...) do
    local t = select(i, ...)
    local iter = getiter(t)
    for k, v in iter(t) do
      rtn[k] = v
    end
  end
  return rtn
end


function lume.concat(...)
  local rtn = {}
  for i = 1, select("#", ...) do
    local t = select(i, ...)
    if t ~= nil then
      local iter = getiter(t)
      for _, v in iter(t) do
        rtn[#rtn + 1] = v
      end
    end
  end
  return rtn
end


function lume.find(t, value)
  local iter = getiter(t)
  for k, v in iter(t) do
    if v == value then return k end
  end
  return nil
end


function lume.match(t, fn)
  fn = iteratee(fn)
  local iter = getiter(t)
  for k, v in iter(t) do
    if fn(v) then return v, k end
  end
  return nil
end


function lume.count(t, fn)
  local count = 0
  local iter = getiter(t)
  if fn then
    fn = iteratee(fn)
    for _, v in iter(t) do
      if fn(v) then count = count + 1 end
    end
  else
    if lume.isarray(t) then
      return #t
    end
    for _ in iter(t) do count = count + 1 end
  end
  return count
end


function lume.slice(t, i, j)
  i = i and absindex(#t, i) or 1
  j = j and absindex(#t, j) or #t
  local rtn = {}
  for x = i < 1 and 1 or i, j > #t and #t or j do
    rtn[#rtn + 1] = t[x]
  end
  return rtn
end


function lume.first(t, n)
  if not n then return t[1] end
  return lume.slice(t, 1, n)
end


function lume.last(t, n)
  if not n then return t[#t] end
  return lume.slice(t, -n, -1)
end


function lume.invert(t)
  local rtn = {}
  for k, v in pairs(t) do rtn[v] = k end
  return rtn
end


function lume.pick(t, ...)
  local rtn = {}
  for i = 1, select("#", ...) do
    local k = select(i, ...)
    rtn[k] = t[k]
  end
  return rtn
end


function lume.keys(t)
  local rtn = {}
  local iter = getiter(t)
  for k in iter(t) do rtn[#rtn + 1] = k end
  return rtn
end


function lume.clone(t)
  local rtn = {}
  for k, v in pairs(t) do rtn[k] = v end
  return rtn
end


function lume.fn(fn, ...)
  assert(iscallable(fn), "expected a function as the first argument")
  local args = { ... }
  return function(...)
    local a = lume.concat(args, { ... })
    return fn(unpack(a))
  end
end


function lume.once(fn, ...)
  local f = lume.fn(fn, ...)
  local done = false
  return function(...)
    if done then return end
    done = true
    return f(...)
  end
end


local memoize_fnkey = {}
local memoize_nil = {}

function lume.memoize(fn)
  local cache = {}
  return function(...)
    local c = cache
    for i = 1, select("#", ...) do
      local a = select(i, ...) or memoize_nil
      c[a] = c[a] or {}
      c = c[a]
    end
    c[memoize_fnkey] = c[memoize_fnkey] or {fn(...)}
    return unpack(c[memoize_fnkey])
  end
end


function lume.combine(...)
  local n = select('#', ...)
  if n == 0 then return noop end
  if n == 1 then
    local fn = select(1, ...)
    if not fn then return noop end
    assert(iscallable(fn), "expected a function or nil")
    return fn
  end
  local funcs = {}
  for i = 1, n do
    local fn = select(i, ...)
    if fn ~= nil then
      assert(iscallable(fn), "expected a function or nil")
      funcs[#funcs + 1] = fn
    end
  end
  return function(...)
    for _, f in ipairs(funcs) do f(...) end
  end
end


function lume.call(fn, ...)
  if fn then
    return fn(...)
  end
end


function lume.time(fn, ...)
  local start = os.clock()
  local rtn = {fn(...)}
  return (os.clock() - start), unpack(rtn)
end


local lambda_cache = {}

function lume.lambda(str)
  if not lambda_cache[str] then
    local args, body = str:match([[^([%w,_ ]-)%->(.-)$]])
    assert(args and body, "bad string lambda")
    local s = "return function(" .. args .. ")\nreturn " .. body .. "\nend"
    lambda_cache[str] = lume.dostring(s)
  end
  return lambda_cache[str]
end


local serialize

local serialize_map = {
  [ "boolean" ] = tostring,
  [ "nil"     ] = tostring,
  [ "string"  ] = function(v) return string.format("%q", v) end,
  [ "number"  ] = function(v)
    if      v ~=  v     then return  "0/0"      --  nan
    elseif  v ==  1 / 0 then return  "1/0"      --  inf
    elseif  v == -1 / 0 then return "-1/0" end  -- -inf
    return tostring(v)
  end,
  [ "table"   ] = function(t, stk)
    stk = stk or {}
    if stk[t] then error("circular reference") end
    local rtn = {}
    stk[t] = true
    for k, v in pairs(t) do
      rtn[#rtn + 1] = "[" .. serialize(k, stk) .. "]=" .. serialize(v, stk)
    end
    stk[t] = nil
    return "{" .. table.concat(rtn, ",") .. "}"
  end
}

setmetatable(serialize_map, {
  __index = function(_, k) error("unsupported serialize type: " .. k) end
})

serialize = function(x, stk)
  return serialize_map[type(x)](x, stk)
end

function lume.serialize(x)
  return serialize(x)
end


function lume.deserialize(str)
  return lume.dostring("return " .. str)
end


function lume.split(str, sep)
  if not sep then
    return lume.array(str:gmatch("([%S]+)"))
  else
    assert(sep ~= "", "empty separator")
    local psep = patternescape(sep)
    return lume.array((str..sep):gmatch("(.-)("..psep..")"))
  end
end


function lume.trim(str, chars)
  if not chars then return str:match("^[%s]*(.-)[%s]*$") end
  chars = patternescape(chars)
  return str:match("^[" .. chars .. "]*(.-)[" .. chars .. "]*$")
end


function lume.wordwrap(str, limit)
  limit = limit or 72
  local check
  if type(limit) == "number" then
    check = function(s) return #s >= limit end
  else
    check = limit
  end
  local rtn = {}
  local line = ""
  for word, spaces in str:gmatch("(%S+)(%s*)") do
    local s = line .. word
    if check(s) then
      table.insert(rtn, line .. "\n")
      line = word
    else
      line = s
    end
    for c in spaces:gmatch(".") do
      if c == "\n" then
        table.insert(rtn, line .. "\n")
        line = ""
      else
        line = line .. c
      end
    end
  end
  table.insert(rtn, line)
  return table.concat(rtn)
end


function lume.format(str, vars)
  if not vars then return str end
  local f = function(x)
    return tostring(vars[x] or vars[tonumber(x)] or "{" .. x .. "}")
  end
  return (str:gsub("{(.-)}", f))
end


function lume.trace(...)
  local info = debug.getinfo(2, "Sl")
  local t = { info.short_src .. ":" .. info.currentline .. ":" }
  for i = 1, select("#", ...) do
    local x = select(i, ...)
    if type(x) == "number" then
      x = string.format("%g", lume.round(x, .01))
    end
    t[#t + 1] = tostring(x)
  end
  print(table.concat(t, " "))
end


function lume.dostring(str)
  return assert((loadstring or load)(str))()
end


function lume.uuid()
  local fn = function(x)
    local r = math.random(16) - 1
    r = (x == "x") and (r + 1) or (r % 4) + 9
    return ("0123456789abcdef"):sub(r, r)
  end
  return (("xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx"):gsub("[xy]", fn))
end


function lume.hotswap(modname)
  local oldglobal = lume.clone(_G)
  local updated = {}
  local function update(old, new)
    if updated[old] then return end
    updated[old] = true
    local oldmt, newmt = getmetatable(old), getmetatable(new)
    if oldmt and newmt then update(oldmt, newmt) end
    for k, v in pairs(new) do
      if type(v) == "table" then update(old[k], v) else old[k] = v end
    end
  end
  local err = nil
  local function onerror(e)
    for k in pairs(_G) do _G[k] = oldglobal[k] end
    err = lume.trim(e)
  end
  local ok, oldmod = pcall(require, modname)
  oldmod = ok and oldmod or nil
  xpcall(function()
    package.loaded[modname] = nil
    local newmod = require(modname)
    if type(oldmod) == "table" then update(oldmod, newmod) end
    for k, v in pairs(oldglobal) do
      if v ~= _G[k] and type(v) == "table" then
        update(v, _G[k])
        _G[k] = v
      end
    end
  end, onerror)
  package.loaded[modname] = oldmod
  if err then return nil, err end
  return oldmod
end


local ripairs_iter = function(t, i)
  i = i - 1
  local v = t[i]
  if v ~= nil then
    return i, v
  end
end

function lume.ripairs(t)
  return ripairs_iter, t, (#t + 1)
end


function lume.color(str, mul)
  mul = mul or 1
  local r, g, b, a
  r, g, b = str:match("#(%x%x)(%x%x)(%x%x)")
  if r then
    r = tonumber(r, 16) / 0xff
    g = tonumber(g, 16) / 0xff
    b = tonumber(b, 16) / 0xff
    a = 1
  elseif str:match("rgba?%s*%([%d%s%.,]+%)") then
    local f = str:gmatch("[%d.]+")
    r = (f() or 0) / 0xff
    g = (f() or 0) / 0xff
    b = (f() or 0) / 0xff
    a = f() or 1
  else
    error(("bad color string '%s'"):format(str))
  end
  return r * mul, g * mul, b * mul, a * mul
end


local chain_mt = {}
chain_mt.__index = lume.map(lume.filter(lume, iscallable, true),
  function(fn)
    return function(self, ...)
      self._value = fn(self._value, ...)
      return self
    end
  end)
chain_mt.__index.result = function(x) return x._value end

function lume.chain(value)
  return setmetatable({ _value = value }, chain_mt)
end

setmetatable(lume,  {
  __call = function(_, ...)
    return lume.chain(...)
  end
})


return lume

end)
__bundle_register("narrator.libs.classic", function(require, _LOADED, __bundle_register, __bundle_modules)
--
-- classic
--
-- Copyright (c) 2014, rxi
--
-- This module is free software; you can redistribute it and/or modify it under
-- the terms of the MIT license. See LICENSE for details.
--


local Object = {}
Object.__index = Object


function Object:new()
end


function Object:extend()
  local cls = {}
  for k, v in pairs(self) do
    if k:find("__") == 1 then
      cls[k] = v
    end
  end
  cls.__index = cls
  cls.super = self
  setmetatable(cls, self)
  return cls
end


function Object:implement(...)
  for _, cls in pairs({...}) do
    for k, v in pairs(cls) do
      if self[k] == nil and type(v) == "function" then
        self[k] = v
      end
    end
  end
end


function Object:is(T)
  local mt = getmetatable(self)
  while mt do
    if mt == T then
      return true
    end
    mt = getmetatable(mt)
  end
  return false
end


function Object:__tostring()
  return "Object"
end


function Object:__call(...)
  local obj = setmetatable({}, self)
  obj:new(...)
  return obj
end


return Object

end)
__bundle_register("narrator.enums", function(require, _LOADED, __bundle_register, __bundle_modules)
--
-- Enums

local enums = {

  -- Release version of Narrator
  engineVersion = 1,

  -- Story item type
  item = { 
    text = 1,
    alts = 2,
    choice = 3,
    condition = 4,
    variable = 5
  },
  
  -- Alternatives sequence type
  sequence = {
    cycle = 1,
    stopping = 2,
    once = 3
  },

  -- Runtime reading mode
  readMode = { 
    text = 1,
    choices = 2,
    gathers = 3,
    quit = 4
  }

}

return enums
end)
__bundle_register("narrator.parser", function(require, _LOADED, __bundle_register, __bundle_modules)
--
-- Dependencies

local lume = require("narrator.libs.lume")
local enums = require("narrator.enums")

-- Safe lpeg requiring
local lpeg = require('narrator.libs.lulpeg')

--
-- LPeg

local S, C, P, V = lpeg.S, lpeg.C, lpeg.P, lpeg.V
local Cb, Ct, Cc, Cg = lpeg.Cb, lpeg.Ct, lpeg.Cc, lpeg.Cg
lpeg.locale(lpeg)

--
-- Parser

local Parser = { }
local Constructor = { }

--- Parse Ink content
-- @param content string: ink content to parse
-- @return table: a book
function Parser.parse(content)

  --
  -- Basic patterns

  local function getLength(array) return #array end

  local eof = -1
  local sp = S(' \t') ^ 0
  local ws = S(' \t\r\n') ^ 0
  local nl = S('\r\n') ^ 1
  local none = Cc(nil)

  local divertSign = P'->'
  local gatherMark = sp * C('-' - divertSign)
  local gatherLevel = Cg(Ct(gatherMark ^ 1) / getLength + none, 'level')
  
  local stickyMarks = Cg(Ct((sp * C('+')) ^ 1) / getLength, 'level') * Cg(Cc(true), 'sticky')
  local choiceMarks = Cg(Ct((sp * C('*')) ^ 1) / getLength, 'level') * Cg(Cc(false), 'sticky')
  local choiceLevel = stickyMarks + choiceMarks

  local id = (lpeg.alpha + '_') * (lpeg.alnum + '_') ^ 0
  local label = Cg('(' * sp * C(id) * sp * ')', 'label')
  local address = id * ('.' * id) ^ -2
  local divert = Cg(divertSign * sp * C(address), 'divert')
  local divertToNothing = divertSign * none
  local tag = '#' * sp * V'text'
  local tags = Cg(Ct(tag * (sp * tag) ^ 0), 'tags')

  local todo = sp * 'TODO:' * (1 - nl) ^ 0
  local commentLine = sp * '//' * sp * (1 - nl) ^ 0
  local commentMulti = sp * '/*' * ((P(1) - '*/') ^ 0) * '*/'
  local comment = commentLine + commentMulti

  local multilineEnd = ws * '}'

  --
  -- Dynamic patterns and evaluation helpers

  local function itemType(type)
    return Cg(Cc(type), 'type')
  end

  local function balancedMultilineItem(isRestricted)
    local isRestricted = isRestricted ~= nil and isRestricted or false
    local paragraph = isRestricted and V'restrictedParagraph' or V'paragraph'
    return sp * paragraph ^ -1 * sp * V'multilineItem' * sp * paragraph ^ -1 * ws
  end

  local function sentenceBefore(excluded, tailed)
    local tailed = tailed or false
    local character = P(1 - S(' \t')) - excluded
    local pattern = (sp * character ^ 1) ^ 1
    local withTail = C(pattern * sp)
    local withoutTail = C(pattern) * sp
    local withoutTailAlways = C(pattern) * sp * #(tags + nl)
    return withoutTailAlways + (tailed and withTail or withoutTail)
  end

  local function unwrapAssignment(assignment)
    local unwrapped = assignment
    unwrapped = unwrapped:gsub('([%w_]*)%s*([%+%-])[%+%-]', '%1 = %1 %2 1')
    unwrapped = unwrapped:gsub('([%w_]*)%s*([%+%-])=%s*(.*)', '%1 = %1 %2 %3')
    local name, value = unwrapped:match('([%w_]*)%s*=%s*(.*)')
    return name or '', value or assignment
  end

  --
  -- Grammar rules

  local inkGrammar = P({ 'root',

    -- Root

    root = ws * V'items' + eof,
    items = Ct(V'item' ^ 0),

    item = balancedMultilineItem() + V'singlelineItem',
    singlelineItem = sp * (V'global' + V'statement' + V'paragraph') * ws,
    multilineItem = ('{' * sp * (V'sequence' + V'switch') * sp * multilineEnd) - V'inlineCondition',

    -- Global declarations

    global =
      Ct(V'inclusion' * itemType('inclusion')) +
      Ct(V'list' * itemType('list')) +
      Ct(V'constant' * itemType('constant')) +
      Ct(V'variable' * itemType('variable'))
    ,

    inclusion = 'INCLUDE ' * sp * Cg(sentenceBefore(nl + comment), 'filename'),
    list = 'LIST ' * sp * V'assignmentPair',
    constant = 'CONST ' * sp * V'assignmentPair',
    variable = 'VAR ' * sp * V'assignmentPair',

    -- Statements

    statement = 
      Ct(V'assignment' * itemType('assignment')) + 
      Ct(V'knot' * itemType('knot')) +
      Ct(V'stitch' * itemType('stitch')) +
      Ct(V'choice' * itemType('choice')) +
      comment + todo
    ,
    
    sectionName = C(id) * sp * P'=' ^ 0,
    knot = P'==' * (P'=' ^ 0) * sp * Cg(V'sectionName', 'knot'),
    stitch = '=' * sp * Cg(V'sectionName', 'stitch'),

    assignment = gatherLevel * sp * '~' * sp * V'assignmentTemp' * sp * V'assignmentPair',
    assignmentTemp = Cg('temp' * Cc(true) + Cc(false), 'temp'),
    assignmentPair = Cg(sentenceBefore(nl + comment) / unwrapAssignment, 'name') * Cg(Cb('name') / 2, 'value'),

    choiceCondition = Cg(V'expression' + none, 'condition'),
    choiceFallback = choiceLevel * sp * V'labelOptional' * sp * V'choiceCondition' * sp * (divert + divertToNothing) * sp * V'tagsOptional',
    choiceNormal = choiceLevel * sp * V'labelOptional' * sp * V'choiceCondition' * sp * Cg(V'text', 'text') * divert ^ -1 * sp * V'tagsOptional',
    choice = V'choiceFallback' + V'choiceNormal',

    -- Paragraph

    paragraph = Ct(gatherLevel * sp * (V'paragraphLabel' + V'paragraphText' + V'paragraphTags') * itemType('paragraph')),
    paragraphLabel = label * sp * Cg(V'textOptional', 'parts') * sp * V'tagsOptional',
    paragraphText = V'labelOptional' * sp * Cg(V'textComplex', 'parts') * sp * V'tagsOptional',
    paragraphTags = V'labelOptional' * sp * Cg(V'textOptional', 'parts') * sp * tags,
    
    labelOptional = label + none,
    textOptional = V'textComplex' + none,
    tagsOptional = tags + none,

    textComplex = Ct((Ct(
      Cg(V'inlineCondition', 'condition') + 
      Cg(V'inlineSequence', 'sequence') + 
      Cg(V'expression', 'expression') +
      Cg(V'text' + ' ', 'text') * (divert ^ -1) + divert
    ) - V'multilineItem') ^ 1),

    text = sentenceBefore(nl + divert + comment + tag + S'{|}', true) - V'statement',

    -- Inline expressions, conditions, sequences

    expression = '{' * sp * sentenceBefore('}' + nl) * sp * '}',

    inlineCondition = '{' * sp * Ct(V'inlineIfElse' + V'inlineIf') * sp * '}',
    inlineIf = Cg(sentenceBefore(S':}' + nl), 'condition') * sp * ':' * sp * Cg(V'textComplex', 'success'),
    inlineIfElse = (V'inlineIf') * sp * '|' * sp * Cg(V'textComplex', 'failure'),
    
    inlineAltEmpty = Ct(Ct(Cg(sp * Cc'', 'text') * sp * divert ^ -1)),
    inlineAlt = V'textComplex' + V'inlineAltEmpty',
    inlineAlts = Ct(((sp * V'inlineAlt' * sp * '|') ^ 1) * sp * V'inlineAlt'),
    inlineSequence = '{' * sp * (
    '!' * sp * Ct(Cg(V'inlineAlts', 'alts') * Cg(Cc('once'), 'sequence')) +
    '&' * sp * Ct(Cg(V'inlineAlts', 'alts') * Cg(Cc('cycle'), 'sequence')) +
    '~' * sp * Ct(Cg(V'inlineAlts', 'alts') * Cg(Cc('stopping'), 'sequence') * Cg(Cc(true),  'shuffle')) +
           Ct(Cg(V'inlineAlts', 'alts') * Cg(Cc('stopping'), 'sequence'))
    ) * sp * '}',

    -- Multiline conditions and switches

    switch = Ct((V'switchComparative' + V'switchConditional') * itemType('switch')),

    switchComparative = Cg(V'switchCondition', 'expression') * ws * Cg(Ct((sp * V'switchCase') ^ 1), 'cases'),
    switchConditional = Cg(Ct(V'switchCasesHeaded' + V'switchCasesOnly'), 'cases'),
    
    switchCasesHeaded = V'switchIf' * ((sp * V'switchCase') ^ 0),
    switchCasesOnly = ws * ((sp * V'switchCase') ^ 1),

    switchIf = Ct(Cg(V'switchCondition', 'condition') * ws * Cg(Ct(V'switchItems'), 'node')),
    switchCase = ('-' - divertSign) * sp * V'switchIf',
    switchCondition = sentenceBefore(':' + nl) * sp * ':' * sp * comment ^ -1,
    switchItems = (V'restrictedItem' - V'switchCase') ^ 1,

    -- Multiline sequences
    
    sequence = Ct((V'sequenceParams' * sp * nl * sp * V'sequenceAlts') * itemType('sequence')),

    sequenceParams = (
      V'sequenceShuffleOptional' * sp * V'sequenceType' +
      V'sequenceShuffle' * sp * V'sequenceType' +
      V'sequenceShuffle' * sp * V'sequenceTypeOptional'
    ) * sp * ':' * sp * comment ^ -1,

    sequenceShuffleOptional = V'sequenceShuffle' + Cg(Cc(false), 'shuffle'),
    sequenceShuffle = Cg(P'shuffle' / function() return true end, 'shuffle'),

    sequenceTypeOptional = V'sequenceType' + Cg(Cc'cycle', 'sequence'),
    sequenceType = Cg(P'cycle' + 'stopping' + 'once', 'sequence'),

    sequenceAlts = Cg(Ct((sp * V'sequenceAlt') ^ 1), 'alts'),
    sequenceAlt = ('-' - divertSign) * ws * Ct(V'sequenceItems'),
    sequenceItems = (V'restrictedItem' - V'sequenceAlt') ^ 1,

    -- Restricted items inside multiline items

    restrictedItem = balancedMultilineItem(true) + V'restrictedSinglelineItem',
    restrictedSinglelineItem = sp * (V'global' + V'restrictedStatement' + V'restrictedParagraph' - multilineEnd) * ws,

    restrictedStatement = Ct(
      V'choice' * itemType('choice') +
      V'assignment' * itemType('assignment')
    ) + comment + todo,
    
    restrictedParagraph = Ct((
      Cg(V'textComplex', 'parts') * sp * V'tagsOptional' +
      Cg(V'textOptional', 'parts') * sp * tags
    ) * itemType('paragraph'))

  })

  --
  -- Result

  local parsedItems = inkGrammar:match(content)
  local book = Constructor.constructBook(parsedItems)
  return book
end

--
-- A book construction

function Constructor.constructBook(items)
  
  local construction = {
    currentKnot = '_',
    currentStitch = '_',
    variablesToCompute = { }
  }

  construction.book = {
    inclusions = { },
    lists = { },
    constants = { },
    variables = { },
    tree = { _ = { _ = { } } }
  }

  construction.book.version = {
    engine = enums.engineVersion,
    tree = 1
  }
  
  construction.nodesChain = {
    construction.book.tree[construction.currentKnot][construction.currentStitch]
  }

  Constructor.addNode(construction, items)
  Constructor.clear(construction.book.tree)
  Constructor.computeVariables(construction)

  return construction.book
end

function Constructor:addNode(items, isRestricted)
  local isRestricted = isRestricted ~= nil and isRestricted or false

  for _, item in ipairs(items) do

    if isRestricted then
      -- Are not allowed inside multiline blocks by Ink rules:
      -- a) nesting levels
      -- b) choices without diverts 

      item.level = nil
      if item.type == 'choice' and item.divert == nil then
        item.type = nil
      end
    end

    if item.type == 'inclusion' then
      -- filename
      Constructor.addInclusion(self, item.filename)
    elseif item.type == 'list' then
      -- name, value
      Constructor.addList(self, item.name, item.value)
    elseif item.type == 'constant' then
      -- name, value
      Constructor.addConstant(self, item.name, item.value)
    elseif item.type == 'variable' then
      -- name, value
      Constructor.addVariable(self, item.name, item.value)
    elseif item.type == 'knot' then
      -- knot
      Constructor.addKnot(self, item.knot)
    elseif item.type == 'stitch' then
      -- stitch
      Constructor.addStitch(self, item.stitch)
    elseif item.type == 'switch' then
      -- expression, cases
      Constructor.addSwitch(self, item.expression, item.cases)
    elseif item.type == 'sequence' then
      -- sequence, shuffle, alts
      Constructor.addSequence(self, item.sequence, item.shuffle, item.alts)
    elseif item.type == 'assignment' then
      -- level, name, value, temp
      Constructor.addAssignment(self, item.level, item.name, item.value, item.temp)
    elseif item.type == 'paragraph' then
      -- level, label, parts, tags
      Constructor.addParagraph(self, item.level, item.label, item.parts, item.tags)
    elseif item.type == 'choice' then
      -- level, sticky, label, condition, text, divert, tags
      Constructor.addChoice(self, item.level, item.sticky, item.label, item.condition, item.text, item.divert, item.tags)
    end
  end
end

function Constructor:addInclusion(filename)
  table.insert(self.book.inclusions, filename)
end

function Constructor:addList(name, value)
  local items = lume.array(value:gmatch('[%w_%.]+'))
  self.book.lists[name] = items

  local switched = lume.array(value:gmatch('%b()'))
  switched = lume.map(switched, function(item) return item:sub(2, #item - 1) end)
  self.book.variables[name] = { [name] = { } }
  lume.each(switched, function(item) self.book.variables[name][name][item] = true end)
end

function Constructor:addConstant(constant, value)
  local value = lume.deserialize(value)
  self.book.constants[constant] = value
end

function Constructor:addVariable(variable, value)
  self.variablesToCompute[variable] = value
end

function Constructor:addKnot(knot)
  self.currentKnot = knot
  self.currentStitch = '_'

  local node = { }
  self.book.tree[self.currentKnot] = { [self.currentStitch] = node }
  self.nodesChain = { node }
end

function Constructor:addStitch(stitch)
  -- If a root stitch is empty we need to add a divert to the first stitch in the ink file.
  if self.currentStitch == '_' then
    local rootStitchNode = self.book.tree[self.currentKnot]._
    if #rootStitchNode == 0 then
      local divertItem = { divert = stitch }
      table.insert(rootStitchNode, divertItem)  
    end
  end

  self.currentStitch = stitch

  local node = { }
  self.book.tree[self.currentKnot][self.currentStitch] = node
  self.nodesChain = { node }
end

function Constructor:addSwitch(expression, cases)
  if expression then
    -- Convert switch cases to comparing conditions with expression
    for _, case in ipairs(cases) do
      if case.condition ~= 'else' then
        case.condition = expression .. '==' .. case.condition
      end
    end
  end

  local item = {
    condition = { },
    success = { }
  }

  for _, case in ipairs(cases) do
    if case.condition == 'else' then
      local failureNode = { }
      table.insert(self.nodesChain, failureNode)
      Constructor.addNode(self, case.node, true)
      table.remove(self.nodesChain)
      item.failure = failureNode
    else
      local successNode = { }
      table.insert(self.nodesChain, successNode)
      Constructor.addNode(self, case.node, true)
      table.remove(self.nodesChain)
      table.insert(item.success, successNode)
      table.insert(item.condition, case.condition)
    end
  end

  Constructor.addItem(self, nil, item)
end

function Constructor:addSequence(sequence, shuffle, alts)
  local item = {
    sequence = sequence,
    shuffle = shuffle and true or nil,
    alts = { }
  }

  for _, alt in ipairs(alts) do
    local altNode = { }
    table.insert(self.nodesChain, altNode)
    Constructor.addNode(self, alt, true)
    table.remove(self.nodesChain)
    table.insert(item.alts, altNode)
  end

  Constructor.addItem(self, nil, item)
end

function Constructor:addAssignment(level, name, value, temp)
  local item = {
    temp = temp or nil,
    var = name,
    value = value
  }

  Constructor.addItem(self, level, item)
end

function Constructor:addParagraph(level, label, parts, tags)
  local items = Constructor.convertParagraphPartsToItems(parts, true)
  items = items or { }
  
  -- If the paragraph has a label or tags we need to place them as the first text item.
  if label ~= nil or tags ~= nil then
    local firstItem

    if #items > 0 and items[1].condition == nil then
      firstItem = items[1]
    else
      firstItem = {  }
      table.insert(items, firstItem)
    end

    firstItem.label = label
    firstItem.tags = tags
  end

  for _, item in ipairs(items) do
    Constructor.addItem(self, level, item)
  end
end

function Constructor.convertParagraphPartsToItems(parts, isRoot)
  if parts == nil then return nil end

  local isRoot = isRoot ~= nil and isRoot or false
  local items = { }
  local item
  
  for index, part in ipairs(parts) do

    if part.condition then -- Inline condition part

      item = {
        condition = part.condition.condition,
        success = Constructor.convertParagraphPartsToItems(part.condition.success),
        failure = Constructor.convertParagraphPartsToItems(part.condition.failure)
      }

      table.insert(items, item)
      item = nil

    elseif part.sequence then -- Inline sequence part
      
      item = {
        sequence = part.sequence.sequence,
        shuffle = part.sequence.shuffle and true or nil,
        alts = { }
      }
      
      for _, alt in ipairs(part.sequence.alts) do
        table.insert(item.alts, Constructor.convertParagraphPartsToItems(alt))
      end

      table.insert(items, item)
      item = nil

    else -- Text, expression and divert may be

      local isDivertOnly = part.divert ~= nil and part.text == nil

      if item == nil then
        item = { text = (isRoot or isDivertOnly) and '' or '<>' }
      end

      if part.text then
        item.text = item.text .. part.text:gsub('%s+', ' ')
      elseif part.expression then
        item.text = item.text .. '#' .. part.expression .. '#'
      end

      if part.divert then
        item.divert = part.divert
        item.text = #item.text > 0 and (item.text .. '<>') or nil
        table.insert(items, item)
        item = nil
      else
        local next = parts[index + 1]
        local nextIsBlock = next and not (next.text or next.expression)

        if not next or nextIsBlock then
          if not isRoot or nextIsBlock then
            item.text = item.text .. '<>'
          end
          table.insert(items, item)
          item = nil  
        end
      end

    end
  end

  if isRoot then
    -- Add a safe prefix and suffix for correct conditions gluing
    
    local firstItem = items[1]
    if firstItem.text == nil and firstItem.divert == nil then
      table.insert(items, 1, { text = '' } )
    end
    
    local lastItem = items[#items]
    if lastItem.text == nil and lastItem.divert == nil then
      table.insert(items, { text = '' } )
    elseif lastItem.text ~= nil and lastItem.divert == nil then
      lastItem.text = lastItem.text:gsub('(.-)%s*$', '%1')
    end
  end

  return items
end

function Constructor:addChoice(level, sticky, label, condition, sentence, divert, tags)
  local item = {
    sticky = sticky or nil,
    condition = condition,
    label = label,
    divert = divert,
    tags = tags
  }

  if sentence == nil then
    item.choice = 0
  else
    local prefix, divider, suffix = sentence:match('(.*)%[(.*)%](.*)')
    prefix = prefix or sentence
    divider = divider or ''
    suffix = suffix or ''

    local text = (prefix .. suffix):gsub('%s+', ' ')
    local choice = (prefix .. divider):gsub('%s+', ' '):gsub('^%s*(.-)%s*$', '%1')

    if divert and #text > 0 and text:match('%S+') then
      text = text .. '<>'
    else
      text = text:gsub('^%s*(.-)%s*$', '%1')
    end
    
    item.text = text
    item.choice = choice
  end

  Constructor.addItem(self, level, item)

  if divert == nil then
    item.node = { }
    table.insert(self.nodesChain, item.node)
  end
end

function Constructor:addItem(level, item)
  local level = (level ~= nil and level > 0) and level or #self.nodesChain
  while #self.nodesChain > level do
    table.remove(self.nodesChain)
  end
  
  local node = self.nodesChain[#self.nodesChain]
  table.insert(node, item)
end

function Constructor:computeVariable(variable, value)
  local constant = self.book.constants[value]
  if constant then
    self.book.variables[variable] = constant
    return
  end

  local listExpression = value:match('%(([%s%w%.,_]*)%)')
  local itemExpressions = listExpression and lume.array(listExpression:gmatch('[%w_%.]+')) or { value }
  local listVariable = listExpression and { } or nil

  for _, itemExpression in ipairs(itemExpressions) do
    local listPart, itemPart = itemExpression:match('([%w_]+)%.([%w_]+)')
    itemPart = itemPart or itemExpression
    
    for listName, listItems in pairs(self.book.lists) do
      local listIsValid = listPart == nil or listPart == listName
      local itemIsFound = lume.find(listItems, itemPart)
      
      if listIsValid and itemIsFound then
        listVariable = listVariable or { }
        listVariable[listName] = listVariable[listName] or { }
        listVariable[listName][itemPart] = true
      end
    end
  end

  if listVariable then
    self.book.variables[variable] = listVariable
  else
    self.book.variables[variable] = lume.deserialize(value)
  end
end

function Constructor:computeVariables()
  for variable, value in pairs(self.variablesToCompute) do
    Constructor.computeVariable(self, variable, value)
  end
end

function Constructor.clear(tree)
  for knot, node in pairs(tree) do
    for stitch, node in pairs(node) do
      Constructor.clearNode(node)
    end
  end
end

function Constructor.clearNode(node)
  for index, item in ipairs(node) do
    
    -- Simplify text only items
    if item.text ~= nil and lume.count(item) == 1 then
      node[index] = item.text
    end
    
    if item.node ~= nil then
      -- Clear choice nodes
      if #item.node == 0 then
        item.node = nil
      else
        Constructor.clearNode(item.node)
      end
      
    end

    if item.success ~= nil then
      -- Simplify single condition
      if type(item.condition) == 'table' and #item.condition == 1 then
        item.condition = item.condition[1]
      end

      -- Clear success nodes
      if item.success[1] ~= nil and item.success[1][1] ~= nil then
        for index, successNode in ipairs(item.success) do
          Constructor.clearNode(successNode)
          if #successNode == 1 and type(successNode[1]) == 'string' then
            item.success[index] = successNode[1]
          end
        end

        if #item.success == 1 then
          item.success = item.success[1]
        end
      else
        Constructor.clearNode(item.success)
        if #item.success == 1 and type(item.success[1]) == 'string' then
          item.success = item.success[1]
        end   
      end

      -- Clear failure nodes
      if item.failure ~= nil then
        Constructor.clearNode(item.failure)
        if #item.failure == 1 and type(item.failure[1]) == 'string' then
          item.failure = item.failure[1]
        end     
      end
    end

    if item.alts ~= nil then
      for index, altNode in ipairs(item.alts) do
        Constructor.clearNode(altNode)
        if #altNode == 1 and type(altNode[1]) == 'string' then
          item.alts[index] = altNode[1]
        end
      end
    end
  end
end

return Parser

end)
__bundle_register("narrator.libs.lulpeg", function(require, _LOADED, __bundle_register, __bundle_modules)
-- LuLPeg, a pure Lua port of LPeg, Roberto Ierusalimschy's
-- Parsing Expression Grammars library.
--
-- Copyright (C) Pierre-Yves Gerardy.
-- Released under the Romantic WTF Public License (cf. the LICENSE
-- file or the end of this file, whichever is present).
--
-- See http://www.inf.puc-rio.br/~roberto/lpeg/ for the original.
--
-- The re.lua module and the test suite (tests/lpeg.*.*.tests.lua)
-- are part of the original LPeg distribution.
local _ENV,       loaded, packages, release, require_
    = _ENV or _G, {},     {},       true,    require

local function l_Require(...)
    local lib = ...

    -- is it a private file?
    if loaded[lib] then
        return loaded[lib]
    elseif packages[lib] then
        loaded[lib] = packages[lib](lib)
        return loaded[lib]
    else
        return require_(lib)
    end
end

--=============================================================================
do local _ENV = _ENV
packages['util'] = function (...)

local getmetatable, setmetatable, load, loadstring, next
    , pairs, pcall, print, rawget, rawset, select, tostring
    , type, unpack
    = getmetatable, setmetatable, load, loadstring, next
    , pairs, pcall, print, rawget, rawset, select, tostring
    , type, unpack
local m, s, t = l_Require"math", l_Require"string", l_Require"table"
local m_max, s_match, s_gsub, t_concat, t_insert
    = m.max, s.match, s.gsub, t.concat, t.insert
local compat = l_Require"compat"
local
function nop () end
local noglobals, getglobal, setglobal if pcall and not compat.lua52 and not release then
    local function errR (_,i)
        error("illegal global read: " .. tostring(i), 2)
    end
    local function errW (_,i, v)
        error("illegal global write: " .. tostring(i)..": "..tostring(v), 2)
    end
    local env = setmetatable({}, { __index=errR, __newindex=errW })
    noglobals = function()
        pcall(setfenv, 3, env)
    end
    function getglobal(k) rawget(env, k) end
    function setglobal(k, v) rawset(env, k, v) end
else
    noglobals = nop
end
local _ENV = noglobals() ------------------------------------------------------
local util = {
    nop = nop,
    noglobals = noglobals,
    getglobal = getglobal,
    setglobal = setglobal
}
util.unpack = t.unpack or unpack
util.pack = t.pack or function(...) return { n = select('#', ...), ... } end
if compat.lua51 then
    local old_load = load
   function util.load (ld, source, mode, env)
     local fun
     if type (ld) == 'string' then
       fun = loadstring (ld)
     else
       fun = old_load (ld, source)
     end
     if env then
       setfenv (fun, env)
     end
     return fun
   end
else
    util.load = load
end
if compat.luajit and compat.jit then
    function util.max (ary)
        local max = 0
        for i = 1, #ary do
            max = m_max(max,ary[i])
        end
        return max
    end
elseif compat.luajit then
    local t_unpack = util.unpack
    function util.max (ary)
     local len = #ary
        if len <=30 or len > 10240 then
            local max = 0
            for i = 1, #ary do
                local j = ary[i]
                if j > max then max = j end
            end
            return max
        else
            return m_max(t_unpack(ary))
        end
    end
else
    local t_unpack = util.unpack
    local safe_len = 1000
    function util.max(array)
        local len = #array
        if len == 0 then return -1 end -- FIXME: shouldn't this be `return -1`?
        local off = 1
        local off_end = safe_len
        local max = array[1] -- seed max.
        repeat
            if off_end > len then off_end = len end
            local seg_max = m_max(t_unpack(array, off, off_end))
            if seg_max > max then
                max = seg_max
            end
            off = off + safe_len
            off_end = off_end + safe_len
        until off >= len
        return max
    end
end
local
function setmode(t,mode)
    local mt = getmetatable(t) or {}
    if mt.__mode then
        error("The mode has already been set on table "..tostring(t)..".")
    end
    mt.__mode = mode
    return setmetatable(t, mt)
end
util.setmode = setmode
function util.weakboth (t)
    return setmode(t,"kv")
end
function util.weakkey (t)
    return setmode(t,"k")
end
function util.weakval (t)
    return setmode(t,"v")
end
function util.strip_mt (t)
    return setmetatable(t, nil)
end
local getuniqueid
do
    local N, index = 0, {}
    function getuniqueid(v)
        if not index[v] then
            N = N + 1
            index[v] = N
        end
        return index[v]
    end
end
util.getuniqueid = getuniqueid
do
    local counter = 0
    function util.gensym ()
        counter = counter + 1
        return "___SYM_"..counter
    end
end
function util.passprint (...) print(...) return ... end
local val_to_str_, key_to_str, table_tostring, cdata_to_str, t_cache
local multiplier = 2
local
function val_to_string (v, indent)
    indent = indent or 0
    t_cache = {} -- upvalue.
    local acc = {}
    val_to_str_(v, acc, indent, indent)
    local res = t_concat(acc, "")
    return res
end
util.val_to_str = val_to_string
function val_to_str_ ( v, acc, indent, str_indent )
    str_indent = str_indent or 1
    if "string" == type( v ) then
        v = s_gsub( v, "\n",  "\n" .. (" "):rep( indent * multiplier + str_indent ) )
        if s_match( s_gsub( v,"[^'\"]",""), '^"+$' ) then
            acc[#acc+1] = t_concat{ "'", "", v, "'" }
        else
            acc[#acc+1] = t_concat{'"', s_gsub(v,'"', '\\"' ), '"' }
        end
    elseif "cdata" == type( v ) then
            cdata_to_str( v, acc, indent )
    elseif "table" == type(v) then
        if t_cache[v] then
            acc[#acc+1] = t_cache[v]
        else
            t_cache[v] = tostring( v )
            table_tostring( v, acc, indent )
        end
    else
        acc[#acc+1] = tostring( v )
    end
end
function key_to_str ( k, acc, indent )
    if "string" == type( k ) and s_match( k, "^[_%a][_%a%d]*$" ) then
        acc[#acc+1] = s_gsub( k, "\n", (" "):rep( indent * multiplier + 1 ) .. "\n" )
    else
        acc[#acc+1] = "[ "
        val_to_str_( k, acc, indent )
        acc[#acc+1] = " ]"
    end
end
function cdata_to_str(v, acc, indent)
    acc[#acc+1] = ( " " ):rep( indent * multiplier )
    acc[#acc+1] = "["
    print(#acc)
    for i = 0, #v do
        if i % 16 == 0 and i ~= 0 then
            acc[#acc+1] = "\n"
            acc[#acc+1] = (" "):rep(indent * multiplier + 2)
        end
        acc[#acc+1] = v[i] and 1 or 0
        acc[#acc+1] = i ~= #v and  ", " or ""
    end
    print(#acc, acc[1], acc[2])
    acc[#acc+1] = "]"
end
function table_tostring ( tbl, acc, indent )
    acc[#acc+1] = t_cache[tbl]
    acc[#acc+1] = "{\n"
    for k, v in pairs( tbl ) do
        local str_indent = 1
        acc[#acc+1] = (" "):rep((indent + 1) * multiplier)
        key_to_str( k, acc, indent + 1)
        if acc[#acc] == " ]"
        and acc[#acc - 2] == "[ "
        then str_indent = 8 + #acc[#acc - 1]
        end
        acc[#acc+1] = " = "
        val_to_str_( v, acc, indent + 1, str_indent)
        acc[#acc+1] = "\n"
    end
    acc[#acc+1] = ( " " ):rep( indent * multiplier )
    acc[#acc+1] = "}"
end
function util.expose(v) print(val_to_string(v)) return v end
function util.map (ary, func, ...)
    if type(ary) == "function" then ary, func = func, ary end
    local res = {}
    for i = 1,#ary do
        res[i] = func(ary[i], ...)
    end
    return res
end
function util.selfmap (ary, func, ...)
    if type(ary) == "function" then ary, func = func, ary end
    for i = 1,#ary do
        ary[i] = func(ary[i], ...)
    end
    return ary
end
local
function map_all (tbl, func, ...)
    if type(tbl) == "function" then tbl, func = func, tbl end
    local res = {}
    for k, v in next, tbl do
        res[k]=func(v, ...)
    end
    return res
end
util.map_all = map_all
local
function fold (ary, func, acc)
    local i0 = 1
    if not acc then
        acc = ary[1]
        i0 = 2
    end
    for i = i0, #ary do
        acc = func(acc,ary[i])
    end
    return acc
end
util.fold = fold
local
function foldr (ary, func, acc)
    local offset = 0
    if not acc then
        acc = ary[#ary]
        offset = 1
    end
    for i = #ary - offset, 1 , -1 do
        acc = func(ary[i], acc)
    end
    return acc
end
util.foldr = foldr
local
function map_fold(ary, mfunc, ffunc, acc)
    local i0 = 1
    if not acc then
        acc = mfunc(ary[1])
        i0 = 2
    end
    for i = i0, #ary do
        acc = ffunc(acc,mfunc(ary[i]))
    end
    return acc
end
util.map_fold = map_fold
local
function map_foldr(ary, mfunc, ffunc, acc)
    local offset = 0
    if not acc then
        acc = mfunc(ary[#acc])
        offset = 1
    end
    for i = #ary - offset, 1 , -1 do
        acc = ffunc(mfunc(ary[i], acc))
    end
    return acc
end
util.map_foldr = map_fold
function util.zip(a1, a2)
    local res, len = {}, m_max(#a1,#a2)
    for i = 1,len do
        res[i] = {a1[i], a2[i]}
    end
    return res
end
function util.zip_all(t1, t2)
    local res = {}
    for k,v in pairs(t1) do
        res[k] = {v, t2[k]}
    end
    for k,v in pairs(t2) do
        if res[k] == nil then
            res[k] = {t1[k], v}
        end
    end
    return res
end
function util.filter(ary,func)
    local res = {}
    for i = 1,#ary do
        if func(ary[i]) then
            t_insert(res, ary[i])
        end
    end
end
local
function id (...) return ... end
util.id = id
local function AND (a,b) return a and b end
local function OR  (a,b) return a or b  end
function util.copy (tbl) return map_all(tbl, id) end
function util.all (ary, mfunc)
    if mfunc then
        return map_fold(ary, mfunc, AND)
    else
        return fold(ary, AND)
    end
end
function util.any (ary, mfunc)
    if mfunc then
        return map_fold(ary, mfunc, OR)
    else
        return fold(ary, OR)
    end
end
function util.get(field)
    return function(tbl) return tbl[field] end
end
function util.lt(ref)
    return function(val) return val < ref end
end
function util.compose(f,g)
    return function(...) return f(g(...)) end
end
function util.extend (destination, ...)
    for i = 1, select('#', ...) do
        for k,v in pairs((select(i, ...))) do
            destination[k] = v
        end
    end
    return destination
end
function util.setify (t)
    local set = {}
    for i = 1, #t do
        set[t[i]]=true
    end
    return set
end
function util.arrayify (...) return {...} end
local
function _checkstrhelper(s)
    return s..""
end
function util.checkstring(s, func)
    local success, str = pcall(_checkstrhelper, s)
    if not success then
        if func == nil then func = "?" end
        error("bad argument to '"
            ..tostring(func)
            .."' (string expected, got "
            ..type(s)
            ..")",
        2)
    end
    return str
end
return util

end
end
--=============================================================================
do local _ENV = _ENV
packages['compiler'] = function (...)
local assert, error, pairs, print, rawset, select, setmetatable, tostring, type
    = assert, error, pairs, print, rawset, select, setmetatable, tostring, type
local s, t, u = l_Require"string", l_Require"table", l_Require"util"
local _ENV = u.noglobals() ----------------------------------------------------
local s_byte, s_sub, t_concat, t_insert, t_remove, t_unpack
    = s.byte, s.sub, t.concat, t.insert, t.remove, u.unpack
local   load,   map,   map_all, t_pack
    = u.load, u.map, u.map_all, u.pack
local expose = u.expose
return function(Builder, LL)
local evaluate, LL_ispattern =  LL.evaluate, LL.ispattern
local charset = Builder.charset
local compilers = {}
local
function compile(pt, ccache)
    if not LL_ispattern(pt) then
        error("pattern expected")
    end
    local typ = pt.pkind
    if typ == "grammar" then
        ccache = {}
    elseif typ == "ref" or typ == "choice" or typ == "sequence" then
        if not ccache[pt] then
            ccache[pt] = compilers[typ](pt, ccache)
        end
        return ccache[pt]
    end
    if not pt.compiled then
        pt.compiled = compilers[pt.pkind](pt, ccache)
    end
    return pt.compiled
end
LL.compile = compile
local
function clear_captures(ary, ci)
    for i = ci, #ary do ary[i] = nil end
end
local LL_compile, LL_evaluate, LL_P
    = LL.compile, LL.evaluate, LL.P
local function computeidex(i, len)
    if i == 0 or i == 1 or i == nil then return 1
    elseif type(i) ~= "number" then error"number or nil expected for the stating index"
    elseif i > 0 then return i > len and len + 1 or i
    else return len + i < 0 and 1 or len + i + 1
    end
end
local function newcaps()
    return {
        kind = {},
        bounds = {},
        openclose = {},
        aux = -- [[DBG]] dbgcaps
            {}
    }
end
local
function _match(dbg, pt, sbj, si, ...)
        if dbg then -------------
            print("@!!! Match !!!@", pt)
        end ---------------------
    pt = LL_P(pt)
    assert(type(sbj) == "string", "string expected for the match subject")
    si = computeidex(si, #sbj)
        if dbg then -------------
            print(("-"):rep(30))
            print(pt.pkind)
            LL.pprint(pt)
        end ---------------------
    local matcher = compile(pt, {})
    local caps = newcaps()
    local matcher_state = {grammars = {}, args = {n = select('#',...),...}, tags = {}}
    local  success, final_si, ci = matcher(sbj, si, caps, 1, matcher_state)
        if dbg then -------------
            print("!!! Done Matching !!! success: ", success,
                "final position", final_si, "final cap index", ci,
                "#caps", #caps.openclose)
        end----------------------
    if success then
        clear_captures(caps.kind, ci)
        clear_captures(caps.aux, ci)
            if dbg then -------------
            print("trimmed cap index = ", #caps + 1)
            LL.cprint(caps, sbj, 1)
            end ---------------------
        local values, _, vi = LL_evaluate(caps, sbj, 1, 1)
            if dbg then -------------
                print("#values", vi)
                expose(values)
            end ---------------------
        if vi == 0
        then return final_si
        else return t_unpack(values, 1, vi) end
    else
        if dbg then print("Failed") end
        return nil
    end
end
function LL.match(...)
    return _match(false, ...)
end
function LL.dmatch(...)
    return _match(true, ...)
end
for _, v in pairs{
    "C", "Cf", "Cg", "Cs", "Ct", "Clb",
    "div_string", "div_table", "div_number", "div_function"
} do
    compilers[v] = load(([=[
    local compile, expose, type, LL = ...
    return function (pt, ccache)
        local matcher, this_aux = compile(pt.pattern, ccache), pt.aux
        return function (sbj, si, caps, ci, state)
            local ref_ci = ci
            local kind, bounds, openclose, aux
                = caps.kind, caps.bounds, caps.openclose, caps.aux
            kind      [ci] = "XXXX"
            bounds    [ci] = si
            openclose [ci] = 0
            caps.aux       [ci] = (this_aux or false)
            local success
            success, si, ci
                = matcher(sbj, si, caps, ci + 1, state)
            if success then
                if ci == ref_ci + 1 then
                    caps.openclose[ref_ci] = si
                else
                    kind      [ci] = "XXXX"
                    bounds    [ci] = si
                    openclose [ci] = ref_ci - ci
                    aux       [ci] = this_aux or false
                    ci = ci + 1
                end
            else
                ci = ci - 1
            end
            return success, si, ci
        end
    end]=]):gsub("XXXX", v), v.." compiler")(compile, expose, type, LL)
end
compilers["Carg"] = function (pt, ccache)
    local n = pt.aux
    return function (sbj, si, caps, ci, state)
        if state.args.n < n then error("reference to absent argument #"..n) end
        caps.kind      [ci] = "value"
        caps.bounds    [ci] = si
        if state.args[n] == nil then
            caps.openclose [ci] = 1/0
            caps.aux       [ci] = 1/0
        else
            caps.openclose [ci] = si
            caps.aux       [ci] = state.args[n]
        end
        return true, si, ci + 1
    end
end
for _, v in pairs{
    "Cb", "Cc", "Cp"
} do
    compilers[v] = load(([=[
    return function (pt, ccache)
        local this_aux = pt.aux
        return function (sbj, si, caps, ci, state)
            caps.kind      [ci] = "XXXX"
            caps.bounds    [ci] = si
            caps.openclose [ci] = si
            caps.aux       [ci] = this_aux or false
            return true, si, ci + 1
        end
    end]=]):gsub("XXXX", v), v.." compiler")(expose)
end
compilers["/zero"] = function (pt, ccache)
    local matcher = compile(pt.pattern, ccache)
    return function (sbj, si, caps, ci, state)
        local success, nsi = matcher(sbj, si, caps, ci, state)
        clear_captures(caps.aux, ci)
        return success, nsi, ci
    end
end
local function pack_Cmt_caps(i,...) return i, t_pack(...) end
compilers["Cmt"] = function (pt, ccache)
    local matcher, func = compile(pt.pattern, ccache), pt.aux
    return function (sbj, si, caps, ci, state)
        local success, Cmt_si, Cmt_ci = matcher(sbj, si, caps, ci, state)
        if not success then
            clear_captures(caps.aux, ci)
            return false, si, ci
        end
        local final_si, values
        if Cmt_ci == ci then
            final_si, values = pack_Cmt_caps(
                func(sbj, Cmt_si, s_sub(sbj, si, Cmt_si - 1))
            )
        else
            clear_captures(caps.aux, Cmt_ci)
            clear_captures(caps.kind, Cmt_ci)
            local cps, _, nn = evaluate(caps, sbj, ci)
                        final_si, values = pack_Cmt_caps(
                func(sbj, Cmt_si, t_unpack(cps, 1, nn))
            )
        end
        if not final_si then
            return false, si, ci
        end
        if final_si == true then final_si = Cmt_si end
        if type(final_si) == "number"
        and si <= final_si
        and final_si <= #sbj + 1
        then
            local kind, bounds, openclose, aux
                = caps.kind, caps.bounds, caps.openclose, caps.aux
            for i = 1, values.n do
                kind      [ci] = "value"
                bounds    [ci] = si
                if values[i] == nil then
                    caps.openclose [ci] = 1/0
                    caps.aux       [ci] = 1/0
                else
                    caps.openclose [ci] = final_si
                    caps.aux       [ci] = values[i]
                end
                ci = ci + 1
            end
        elseif type(final_si) == "number" then
            error"Index out of bounds returned by match-time capture."
        else
            error("Match time capture must return a number, a boolean or nil"
                .." as first argument, or nothing at all.")
        end
        return true, final_si, ci
    end
end
compilers["string"] = function (pt, ccache)
    local S = pt.aux
    local N = #S
    return function(sbj, si, caps, ci, state)
        local in_1 = si - 1
        for i = 1, N do
            local c
            c = s_byte(sbj,in_1 + i)
            if c ~= S[i] then
                return false, si, ci
            end
        end
        return true, si + N, ci
    end
end
compilers["char"] = function (pt, ccache)
    return load(([=[
        local s_byte, s_char = ...
        return function(sbj, si, caps, ci, state)
            local c, nsi = s_byte(sbj, si), si + 1
            if c ~= __C0__ then
                return false, si, ci
            end
            return true, nsi, ci
        end]=]):gsub("__C0__", tostring(pt.aux)))(s_byte, ("").char)
end
local
function truecompiled (sbj, si, caps, ci, state)
    return true, si, ci
end
compilers["true"] = function (pt)
    return truecompiled
end
local
function falsecompiled (sbj, si, caps, ci, state)
    return false, si, ci
end
compilers["false"] = function (pt)
    return falsecompiled
end
local
function eoscompiled (sbj, si, caps, ci, state)
    return si > #sbj, si, ci
end
compilers["eos"] = function (pt)
    return eoscompiled
end
local
function onecompiled (sbj, si, caps, ci, state)
    local char, _ = s_byte(sbj, si), si + 1
    if char
    then return true, si + 1, ci
    else return false, si, ci end
end
compilers["one"] = function (pt)
    return onecompiled
end
compilers["any"] = function (pt)
    local N = pt.aux
    if N == 1 then
        return onecompiled
    else
        N = pt.aux - 1
        return function (sbj, si, caps, ci, state)
            local n = si + N
            if n <= #sbj then
                return true, n + 1, ci
            else
                return false, si, ci
            end
        end
    end
end
do
    local function checkpatterns(g)
        for k,v in pairs(g.aux) do
            if not LL_ispattern(v) then
                error(("rule 'A' is not a pattern"):gsub("A", tostring(k)))
            end
        end
    end
    compilers["grammar"] = function (pt, ccache)
        checkpatterns(pt)
        local gram = map_all(pt.aux, compile, ccache)
        local start = gram[1]
        return function (sbj, si, caps, ci, state)
            t_insert(state.grammars, gram)
            local success, nsi, ci = start(sbj, si, caps, ci, state)
            t_remove(state.grammars)
            return success, nsi, ci
        end
    end
end
local dummy_acc = {kind={}, bounds={}, openclose={}, aux={}}
compilers["behind"] = function (pt, ccache)
    local matcher, N = compile(pt.pattern, ccache), pt.aux
    return function (sbj, si, caps, ci, state)
        if si <= N then return false, si, ci end
        local success = matcher(sbj, si - N, dummy_acc, ci, state)
        dummy_acc.aux = {}
        return success, si, ci
    end
end
compilers["range"] = function (pt)
    local ranges = pt.aux
    return function (sbj, si, caps, ci, state)
        local char, nsi = s_byte(sbj, si), si + 1
        for i = 1, #ranges do
            local r = ranges[i]
            if char and r[char]
            then return true, nsi, ci end
        end
        return false, si, ci
    end
end
compilers["set"] = function (pt)
    local s = pt.aux
    return function (sbj, si, caps, ci, state)
        local char, nsi = s_byte(sbj, si), si + 1
        if s[char]
        then return true, nsi, ci
        else return false, si, ci end
    end
end
compilers["range"] = compilers.set
compilers["ref"] = function (pt, ccache)
    local name = pt.aux
    local ref
    return function (sbj, si, caps, ci, state)
        if not ref then
            if #state.grammars == 0 then
                error(("rule 'XXXX' used outside a grammar"):gsub("XXXX", tostring(name)))
            elseif not state.grammars[#state.grammars][name] then
                error(("rule 'XXXX' undefined in given grammar"):gsub("XXXX", tostring(name)))
            end
            ref = state.grammars[#state.grammars][name]
        end
            local success, nsi, nci = ref(sbj, si, caps, ci, state)
        return success, nsi, nci
    end
end
local choice_tpl = [=[
            success, si, ci = XXXX(sbj, si, caps, ci, state)
            if success then
                return true, si, ci
            else
            end]=]
local function flatten(kind, pt, ccache)
    if pt[2].pkind == kind then
        return compile(pt[1], ccache), flatten(kind, pt[2], ccache)
    else
        return compile(pt[1], ccache), compile(pt[2], ccache)
    end
end
compilers["choice"] = function (pt, ccache)
    local choices = {flatten("choice", pt, ccache)}
    local names, chunks = {}, {}
    for i = 1, #choices do
        local m = "ch"..i
        names[#names + 1] = m
        chunks[ #names  ] = choice_tpl:gsub("XXXX", m)
    end
    names[#names + 1] = "clear_captures"
    choices[ #names ] = clear_captures
    local compiled = t_concat{
        "local ", t_concat(names, ", "), [=[ = ...
        return function (sbj, si, caps, ci, state)
            local aux, success = caps.aux, false
            ]=],
            t_concat(chunks,"\n"),[=[--
            return false, si, ci
        end]=]
    }
    return load(compiled, "Choice")(t_unpack(choices))
end
local sequence_tpl = [=[
            success, si, ci = XXXX(sbj, si, caps, ci, state)
            if not success then
                return false, ref_si, ref_ci
            end]=]
compilers["sequence"] = function (pt, ccache)
    local sequence = {flatten("sequence", pt, ccache)}
    local names, chunks = {}, {}
    for i = 1, #sequence do
        local m = "seq"..i
        names[#names + 1] = m
        chunks[ #names  ] = sequence_tpl:gsub("XXXX", m)
    end
    names[#names + 1] = "clear_captures"
    sequence[ #names ] = clear_captures
    local compiled = t_concat{
        "local ", t_concat(names, ", "), [=[ = ...
        return function (sbj, si, caps, ci, state)
            local ref_si, ref_ci, success = si, ci
            ]=],
            t_concat(chunks,"\n"),[=[
            return true, si, ci
        end]=]
    }
   return load(compiled, "Sequence")(t_unpack(sequence))
end
compilers["at most"] = function (pt, ccache)
    local matcher, n = compile(pt.pattern, ccache), pt.aux
    n = -n
    return function (sbj, si, caps, ci, state)
        local success = true
        for i = 1, n do
            success, si, ci = matcher(sbj, si, caps, ci, state)
            if not success then
                break
            end
        end
        return true, si, ci
    end
end
compilers["at least"] = function (pt, ccache)
    local matcher, n = compile(pt.pattern, ccache), pt.aux
    if n == 0 then
        return function (sbj, si, caps, ci, state)
            local last_si, last_ci
            while true do
                local success
                last_si, last_ci = si, ci
                success, si, ci = matcher(sbj, si, caps, ci, state)
                if not success then
                    si, ci = last_si, last_ci
                    break
                end
            end
            return true, si, ci
        end
    elseif n == 1 then
        return function (sbj, si, caps, ci, state)
            local last_si, last_ci
            local success = true
            success, si, ci = matcher(sbj, si, caps, ci, state)
            if not success then
                return false, si, ci
            end
            while true do
                local success
                last_si, last_ci = si, ci
                success, si, ci = matcher(sbj, si, caps, ci, state)
                if not success then
                    si, ci = last_si, last_ci
                    break
                end
            end
            return true, si, ci
        end
    else
        return function (sbj, si, caps, ci, state)
            local last_si, last_ci
            local success = true
            for _ = 1, n do
                success, si, ci = matcher(sbj, si, caps, ci, state)
                if not success then
                    return false, si, ci
                end
            end
            while true do
                local success
                last_si, last_ci = si, ci
                success, si, ci = matcher(sbj, si, caps, ci, state)
                if not success then
                    si, ci = last_si, last_ci
                    break
                end
            end
            return true, si, ci
        end
    end
end
compilers["unm"] = function (pt, ccache)
    if pt.pkind == "any" and pt.aux == 1 then
        return eoscompiled
    end
    local matcher = compile(pt.pattern, ccache)
    return function (sbj, si, caps, ci, state)
        local success, _, _ = matcher(sbj, si, caps, ci, state)
        return not success, si, ci
    end
end
compilers["lookahead"] = function (pt, ccache)
    local matcher = compile(pt.pattern, ccache)
    return function (sbj, si, caps, ci, state)
        local success, _, _ = matcher(sbj, si, caps, ci, state)
        return success, si, ci
    end
end
end

end
end
--=============================================================================
do local _ENV = _ENV
packages['datastructures'] = function (...)
local getmetatable, pairs, setmetatable, type
    = getmetatable, pairs, setmetatable, type
local m, t , u = l_Require"math", l_Require"table", l_Require"util"
local compat = l_Require"compat"
local ffi if compat.luajit then
    ffi = l_Require"ffi"
end
local _ENV = u.noglobals() ----------------------------------------------------
local   extend,   load, u_max
    = u.extend, u.load, u.max
local m_max, t_concat, t_insert, t_sort
    = m.max, t.concat, t.insert, t.sort
local structfor = {}
local byteset_new, isboolset, isbyteset
local byteset_mt = {}
local
function byteset_constructor (upper)
    local set = setmetatable(load(t_concat{
        "return{ [0]=false",
        (", false"):rep(upper),
        " }"
    })(),
    byteset_mt)
    return set
end
if compat.jit then
    local struct, boolset_constructor = {v={}}
    function byteset_mt.__index(s,i)
        if i == nil or i > s.upper then return nil end
        return s.v[i]
    end
    function byteset_mt.__len(s)
        return s.upper
    end
    function byteset_mt.__newindex(s,i,v)
        s.v[i] = v
    end
    boolset_constructor = ffi.metatype('struct { int upper; bool v[?]; }', byteset_mt)
    function byteset_new (t)
        if type(t) == "number" then
            local res = boolset_constructor(t+1)
            res.upper = t
            return res
        end
        local upper = u_max(t)
        struct.upper = upper
        if upper > 255 then error"bool_set overflow" end
        local set = boolset_constructor(upper+1)
        set.upper = upper
        for i = 1, #t do set[t[i]] = true end
        return set
    end
    function isboolset(s) return type(s)=="cdata" and ffi.istype(s, boolset_constructor) end
    isbyteset = isboolset
else
    function byteset_new (t)
        if type(t) == "number" then return byteset_constructor(t) end
        local set = byteset_constructor(u_max(t))
        for i = 1, #t do set[t[i]] = true end
        return set
    end
    function isboolset(s) return false end
    function isbyteset (s)
        return getmetatable(s) == byteset_mt
    end
end
local
function byterange_new (low, high)
    high = ( low <= high ) and high or -1
    local set = byteset_new(high)
    for i = low, high do
        set[i] = true
    end
    return set
end
local tmpa, tmpb ={}, {}
local
function set_if_not_yet (s, dest)
    if type(s) == "number" then
        dest[s] = true
        return dest
    else
        return s
    end
end
local
function clean_ab (a,b)
    tmpa[a] = nil
    tmpb[b] = nil
end
local
function byteset_union (a ,b)
    local upper = m_max(
        type(a) == "number" and a or #a,
        type(b) == "number" and b or #b
    )
    local A, B
        = set_if_not_yet(a, tmpa)
        , set_if_not_yet(b, tmpb)
    local res = byteset_new(upper)
    for i = 0, upper do
        res[i] = A[i] or B[i] or false
    end
    clean_ab(a,b)
    return res
end
local
function byteset_difference (a, b)
    local res = {}
    for i = 0, 255 do
        res[i] = a[i] and not b[i]
    end
    return res
end
local
function byteset_tostring (s)
    local list = {}
    for i = 0, 255 do
        list[#list+1] = (s[i] == true) and i or nil
    end
    return t_concat(list,", ")
end
structfor.binary = {
    set ={
        new = byteset_new,
        union = byteset_union,
        difference = byteset_difference,
        tostring = byteset_tostring
    },
    Range = byterange_new,
    isboolset = isboolset,
    isbyteset = isbyteset,
    isset = isbyteset
}
local set_mt = {}
local
function set_new (t)
    local set = setmetatable({}, set_mt)
    for i = 1, #t do set[t[i]] = true end
    return set
end
local -- helper for the union code.
function add_elements(a, res)
    for k in pairs(a) do res[k] = true end
    return res
end
local
function set_union (a, b)
    a, b = (type(a) == "number") and set_new{a} or a
         , (type(b) == "number") and set_new{b} or b
    local res = set_new{}
    add_elements(a, res)
    add_elements(b, res)
    return res
end
local
function set_difference(a, b)
    local list = {}
    a, b = (type(a) == "number") and set_new{a} or a
         , (type(b) == "number") and set_new{b} or b
    for el in pairs(a) do
        if a[el] and not b[el] then
            list[#list+1] = el
        end
    end
    return set_new(list)
end
local
function set_tostring (s)
    local list = {}
    for el in pairs(s) do
        t_insert(list,el)
    end
    t_sort(list)
    return t_concat(list, ",")
end
local
function isset (s)
    return (getmetatable(s) == set_mt)
end
local
function range_new (start, finish)
    local list = {}
    for i = start, finish do
        list[#list + 1] = i
    end
    return set_new(list)
end
structfor.other = {
    set = {
        new = set_new,
        union = set_union,
        tostring = set_tostring,
        difference = set_difference,
    },
    Range = range_new,
    isboolset = isboolset,
    isbyteset = isbyteset,
    isset = isset,
    isrange = function(a) return false end
}
return function(Builder, LL)
    local cs = (Builder.options or {}).charset or "binary"
    if type(cs) == "string" then
        cs = (cs == "binary") and "binary" or "other"
    else
        cs = cs.binary and "binary" or "other"
    end
    return extend(Builder, structfor[cs])
end

end
end
--=============================================================================
do local _ENV = _ENV
packages['re'] = function (...)

return function(Builder, LL)
local tonumber, type, print, error = tonumber, type, print, error
local setmetatable = setmetatable
local m = LL
local mm = m
local mt = getmetatable(mm.P(0))
local version = _VERSION
if version == "Lua 5.2" then _ENV = nil end
local any = m.P(1)
local Predef = { nl = m.P"\n" }
local mem
local fmem
local gmem
local function updatelocale ()
  mm.locale(Predef)
  Predef.a = Predef.alpha
  Predef.c = Predef.cntrl
  Predef.d = Predef.digit
  Predef.g = Predef.graph
  Predef.l = Predef.lower
  Predef.p = Predef.punct
  Predef.s = Predef.space
  Predef.u = Predef.upper
  Predef.w = Predef.alnum
  Predef.x = Predef.xdigit
  Predef.A = any - Predef.a
  Predef.C = any - Predef.c
  Predef.D = any - Predef.d
  Predef.G = any - Predef.g
  Predef.L = any - Predef.l
  Predef.P = any - Predef.p
  Predef.S = any - Predef.s
  Predef.U = any - Predef.u
  Predef.W = any - Predef.w
  Predef.X = any - Predef.x
  mem = {}    -- restart memoization
  fmem = {}
  gmem = {}
  local mt = {__mode = "v"}
  setmetatable(mem, mt)
  setmetatable(fmem, mt)
  setmetatable(gmem, mt)
end
updatelocale()
local function getdef (id, defs)
  local c = defs and defs[id]
  if not c then error("undefined name: " .. id) end
  return c
end
local function patt_error (s, i)
  local msg = (#s < i + 20) and s:sub(i)
                             or s:sub(i,i+20) .. "..."
  msg = ("pattern error near '%s'"):format(msg)
  error(msg, 2)
end
local function mult (p, n)
  local np = mm.P(true)
  while n >= 1 do
    if n%2 >= 1 then np = np * p end
    p = p * p
    n = n/2
  end
  return np
end
local function equalcap (s, i, c)
  if type(c) ~= "string" then return nil end
  local e = #c + i
  if s:sub(i, e - 1) == c then return e else return nil end
end
local S = (Predef.space + "--" * (any - Predef.nl)^0)^0
local name = m.R("AZ", "az", "__") * m.R("AZ", "az", "__", "09")^0
local arrow = S * "<-"
local seq_follow = m.P"/" + ")" + "}" + ":}" + "~}" + "|}" + (name * arrow) + -1
name = m.C(name)
local Def = name * m.Carg(1)
local num = m.C(m.R"09"^1) * S / tonumber
local String = "'" * m.C((any - "'")^0) * "'" +
               '"' * m.C((any - '"')^0) * '"'
local defined = "%" * Def / function (c,Defs)
  local cat =  Defs and Defs[c] or Predef[c]
  if not cat then error ("name '" .. c .. "' undefined") end
  return cat
end
local Range = m.Cs(any * (m.P"-"/"") * (any - "]")) / mm.R
local item = defined + Range + m.C(any)
local Class =
    "["
  * (m.C(m.P"^"^-1))    -- optional complement symbol
  * m.Cf(item * (item - "]")^0, mt.__add) /
                          function (c, p) return c == "^" and any - p or p end
  * "]"
local function adddef (t, k, exp)
  if t[k] then
    error("'"..k.."' already defined as a rule")
  else
    t[k] = exp
  end
  return t
end
local function firstdef (n, r) return adddef({n}, n, r) end
local function NT (n, b)
  if not b then
    error("rule '"..n.."' used outside a grammar")
  else return mm.V(n)
  end
end
local exp = m.P{ "Exp",
  Exp = S * ( m.V"Grammar"
            + m.Cf(m.V"Seq" * ("/" * S * m.V"Seq")^0, mt.__add) );
  Seq = m.Cf(m.Cc(m.P"") * m.V"Prefix"^0 , mt.__mul)
        * (m.L(seq_follow) + patt_error);
  Prefix = "&" * S * m.V"Prefix" / mt.__len
         + "!" * S * m.V"Prefix" / mt.__unm
         + m.V"Suffix";
  Suffix = m.Cf(m.V"Primary" * S *
          ( ( m.P"+" * m.Cc(1, mt.__pow)
            + m.P"*" * m.Cc(0, mt.__pow)
            + m.P"?" * m.Cc(-1, mt.__pow)
            + "^" * ( m.Cg(num * m.Cc(mult))
                    + m.Cg(m.C(m.S"+-" * m.R"09"^1) * m.Cc(mt.__pow))
                    )
            + "->" * S * ( m.Cg((String + num) * m.Cc(mt.__div))
                         + m.P"{}" * m.Cc(nil, m.Ct)
                         + m.Cg(Def / getdef * m.Cc(mt.__div))
                         )
            + "=>" * S * m.Cg(Def / getdef * m.Cc(m.Cmt))
            ) * S
          )^0, function (a,b,f) return f(a,b) end );
  Primary = "(" * m.V"Exp" * ")"
            + String / mm.P
            + Class
            + defined
            + "{:" * (name * ":" + m.Cc(nil)) * m.V"Exp" * ":}" /
                     function (n, p) return mm.Cg(p, n) end
            + "=" * name / function (n) return mm.Cmt(mm.Cb(n), equalcap) end
            + m.P"{}" / mm.Cp
            + "{~" * m.V"Exp" * "~}" / mm.Cs
            + "{|" * m.V"Exp" * "|}" / mm.Ct
            + "{" * m.V"Exp" * "}" / mm.C
            + m.P"." * m.Cc(any)
            + (name * -arrow + "<" * name * ">") * m.Cb("G") / NT;
  Definition = name * arrow * m.V"Exp";
  Grammar = m.Cg(m.Cc(true), "G") *
            m.Cf(m.V"Definition" / firstdef * m.Cg(m.V"Definition")^0,
              adddef) / mm.P
}
local pattern = S * m.Cg(m.Cc(false), "G") * exp / mm.P * (-any + patt_error)
local function compile (p, defs)
  if mm.type(p) == "pattern" then return p end   -- already compiled
  local cp = pattern:match(p, 1, defs)
  if not cp then error("incorrect pattern", 3) end
  return cp
end
local function match (s, p, i)
  local cp = mem[p]
  if not cp then
    cp = compile(p)
    mem[p] = cp
  end
  return cp:match(s, i or 1)
end
local function find (s, p, i)
  local cp = fmem[p]
  if not cp then
    cp = compile(p) / 0
    cp = mm.P{ mm.Cp() * cp * mm.Cp() + 1 * mm.V(1) }
    fmem[p] = cp
  end
  local i, e = cp:match(s, i or 1)
  if i then return i, e - 1
  else return i
  end
end
local function gsub (s, p, rep)
  local g = gmem[p] or {}   -- ensure gmem[p] is not collected while here
  gmem[p] = g
  local cp = g[rep]
  if not cp then
    cp = compile(p)
    cp = mm.Cs((cp / rep + 1)^0)
    g[rep] = cp
  end
  return cp:match(s)
end
local re = {
  compile = compile,
  match = match,
  find = find,
  gsub = gsub,
  updatelocale = updatelocale,
}
return re
end
end
end
--=============================================================================
do local _ENV = _ENV
packages['charsets'] = function (...)

local s, t, u = l_Require"string", l_Require"table", l_Require"util"
local _ENV = u.noglobals() ----------------------------------------------------
local copy = u.copy
local s_char, s_sub, s_byte, t_concat, t_insert
    = s.char, s.sub, s.byte, t.concat, t.insert
local
function utf8_offset (byte)
    if byte < 128 then return 0, byte
    elseif byte < 192 then
        error("Byte values between 0x80 to 0xBF cannot start a multibyte sequence")
    elseif byte < 224 then return 1, byte - 192
    elseif byte < 240 then return 2, byte - 224
    elseif byte < 248 then return 3, byte - 240
    elseif byte < 252 then return 4, byte - 248
    elseif byte < 254 then return 5, byte - 252
    else
        error("Byte values between 0xFE and OxFF cannot start a multibyte sequence")
    end
end
local
function utf8_validate (subject, start, finish)
    start = start or 1
    finish = finish or #subject
    local offset, char
        = 0
    for i = start,finish do
        local b = s_byte(subject,i)
        if offset == 0 then
            char = i
            local success
            success, offset = pcall(utf8_offset, b)
            if not success then return false, char - 1 end
        else
            if not (127 < b and b < 192) then
                return false, char - 1
            end
            offset = offset -1
        end
    end
    if offset ~= 0 then return nil, char - 1 end -- Incomplete input.
    return true, finish
end
local
function utf8_next_int (subject, i)
    i = i and i+1 or 1
    if i > #subject then return end
    local c = s_byte(subject, i)
    local offset, val = utf8_offset(c)
    for i = i+1, i+offset do
        c = s_byte(subject, i)
        val = val * 64 + (c-128)
    end
  return i + offset, i, val
end
local
function utf8_next_char (subject, i)
    i = i and i+1 or 1
    if i > #subject then return end
    local offset = utf8_offset(s_byte(subject,i))
    return i + offset, i, s_sub(subject, i, i + offset)
end
local
function utf8_split_int (subject)
    local chars = {}
    for _, _, c in utf8_next_int, subject do
        t_insert(chars,c)
    end
    return chars
end
local
function utf8_split_char (subject)
    local chars = {}
    for _, _, c in utf8_next_char, subject do
        t_insert(chars,c)
    end
    return chars
end
local
function utf8_get_int(subject, i)
    if i > #subject then return end
    local c = s_byte(subject, i)
    local offset, val = utf8_offset(c)
    for i = i+1, i+offset do
        c = s_byte(subject, i)
        val = val * 64 + ( c - 128 )
    end
    return val, i + offset + 1
end
local
function split_generator (get)
    if not get then return end
    return function(subject)
        local res = {}
        local o, i = true
        while o do
            o,i = get(subject, i)
            res[#res] = o
        end
        return res
    end
end
local
function merge_generator (char)
    if not char then return end
    return function(ary)
        local res = {}
        for i = 1, #ary do
            t_insert(res,char(ary[i]))
        end
        return t_concat(res)
    end
end
local
function utf8_get_int2 (subject, i)
    local byte, b5, b4, b3, b2, b1 = s_byte(subject, i)
    if byte < 128 then return byte, i + 1
    elseif byte < 192 then
        error("Byte values between 0x80 to 0xBF cannot start a multibyte sequence")
    elseif byte < 224 then
        return (byte - 192)*64 + s_byte(subject, i+1), i+2
    elseif byte < 240 then
            b2, b1 = s_byte(subject, i+1, i+2)
        return (byte-224)*4096 + b2%64*64 + b1%64, i+3
    elseif byte < 248 then
        b3, b2, b1 = s_byte(subject, i+1, i+2, 1+3)
        return (byte-240)*262144 + b3%64*4096 + b2%64*64 + b1%64, i+4
    elseif byte < 252 then
        b4, b3, b2, b1 = s_byte(subject, i+1, i+2, 1+3, i+4)
        return (byte-248)*16777216 + b4%64*262144 + b3%64*4096 + b2%64*64 + b1%64, i+5
    elseif byte < 254 then
        b5, b4, b3, b2, b1 = s_byte(subject, i+1, i+2, 1+3, i+4, i+5)
        return (byte-252)*1073741824 + b5%64*16777216 + b4%64*262144 + b3%64*4096 + b2%64*64 + b1%64, i+6
    else
        error("Byte values between 0xFE and OxFF cannot start a multibyte sequence")
    end
end
local
function utf8_get_char(subject, i)
    if i > #subject then return end
    local offset = utf8_offset(s_byte(subject,i))
    return s_sub(subject, i, i + offset), i + offset + 1
end
local
function utf8_char(c)
    if     c < 128 then
        return                                                                               s_char(c)
    elseif c < 2048 then
        return                                                          s_char(192 + c/64, 128 + c%64)
    elseif c < 55296 or 57343 < c and c < 65536 then
        return                                         s_char(224 + c/4096, 128 + c/64%64, 128 + c%64)
    elseif c < 2097152 then
        return                      s_char(240 + c/262144, 128 + c/4096%64, 128 + c/64%64, 128 + c%64)
    elseif c < 67108864 then
        return s_char(248 + c/16777216, 128 + c/262144%64, 128 + c/4096%64, 128 + c/64%64, 128 + c%64)
    elseif c < 2147483648 then
        return s_char( 252 + c/1073741824,
                   128 + c/16777216%64, 128 + c/262144%64, 128 + c/4096%64, 128 + c/64%64, 128 + c%64)
    end
    error("Bad Unicode code point: "..c..".")
end
local
function binary_validate (subject, start, finish)
    start = start or 1
    finish = finish or #subject
    return true, finish
end
local
function binary_next_int (subject, i)
    i = i and i+1 or 1
    if i >= #subject then return end
    return i, i, s_sub(subject, i, i)
end
local
function binary_next_char (subject, i)
    i = i and i+1 or 1
    if i > #subject then return end
    return i, i, s_byte(subject,i)
end
local
function binary_split_int (subject)
    local chars = {}
    for i = 1, #subject do
        t_insert(chars, s_byte(subject,i))
    end
    return chars
end
local
function binary_split_char (subject)
    local chars = {}
    for i = 1, #subject do
        t_insert(chars, s_sub(subject,i,i))
    end
    return chars
end
local
function binary_get_int(subject, i)
    return s_byte(subject, i), i + 1
end
local
function binary_get_char(subject, i)
    return s_sub(subject, i, i), i + 1
end
local charsets = {
    binary = {
        name = "binary",
        binary = true,
        validate   = binary_validate,
        split_char = binary_split_char,
        split_int  = binary_split_int,
        next_char  = binary_next_char,
        next_int   = binary_next_int,
        get_char   = binary_get_char,
        get_int    = binary_get_int,
        tochar    = s_char
    },
    ["UTF-8"] = {
        name = "UTF-8",
        validate   = utf8_validate,
        split_char = utf8_split_char,
        split_int  = utf8_split_int,
        next_char  = utf8_next_char,
        next_int   = utf8_next_int,
        get_char   = utf8_get_char,
        get_int    = utf8_get_int
    }
}
return function (Builder)
    local cs = Builder.options.charset or "binary"
    if charsets[cs] then
        Builder.charset = copy(charsets[cs])
        Builder.binary_split_int = binary_split_int
    else
        error("NYI: custom charsets")
    end
end

end
end
--=============================================================================
do local _ENV = _ENV
packages['evaluator'] = function (...)

local select, tonumber, tostring, type
    = select, tonumber, tostring, type
local s, t, u = l_Require"string", l_Require"table", l_Require"util"
local s_sub, t_concat
    = s.sub, t.concat
local t_unpack
    = u.unpack
local _ENV = u.noglobals() ----------------------------------------------------
return function(Builder, LL) -- Decorator wrapper
local eval = {}
local
function insert (caps, sbj, vals, ci, vi)
    local openclose, kind = caps.openclose, caps.kind
    while kind[ci] and openclose[ci] >= 0 do
        ci, vi = eval[kind[ci]](caps, sbj, vals, ci, vi)
    end
    return ci, vi
end
function eval.C (caps, sbj, vals, ci, vi)
    if caps.openclose[ci] > 0 then
        vals[vi] = s_sub(sbj, caps.bounds[ci], caps.openclose[ci] - 1)
        return ci + 1, vi + 1
    end
    vals[vi] = false -- pad it for now
    local cj, vj = insert(caps, sbj, vals, ci + 1, vi + 1)
    vals[vi] = s_sub(sbj, caps.bounds[ci], caps.bounds[cj] - 1)
    return cj + 1, vj
end
local
function lookback (caps, label, ci)
    local aux, openclose, kind= caps.aux, caps.openclose, caps.kind
    repeat
        ci = ci - 1
        local auxv, oc = aux[ci], openclose[ci]
        if oc < 0 then ci = ci + oc end
        if oc ~= 0 and kind[ci] == "Clb" and label == auxv then
            return ci
        end
    until ci == 1
    label = type(label) == "string" and "'"..label.."'" or tostring(label)
    error("back reference "..label.." not found")
end
function eval.Cb (caps, sbj, vals, ci, vi)
    local Cb_ci = lookback(caps, caps.aux[ci], ci)
    Cb_ci, vi = eval.Cg(caps, sbj, vals, Cb_ci, vi)
    return ci + 1, vi
end
function eval.Cc (caps, sbj, vals, ci, vi)
    local these_values = caps.aux[ci]
    for i = 1, these_values.n do
        vi, vals[vi] = vi + 1, these_values[i]
    end
    return ci + 1, vi
end
eval["Cf"] = function() error("NYI: Cf") end
function eval.Cf (caps, sbj, vals, ci, vi)
    if caps.openclose[ci] > 0 then
        error"No First Value"
    end
    local func, Cf_vals, Cf_vi = caps.aux[ci], {}
    ci = ci + 1
    ci, Cf_vi = eval[caps.kind[ci]](caps, sbj, Cf_vals, ci, 1)
    if Cf_vi == 1 then
        error"No first value"
    end
    local result = Cf_vals[1]
    while caps.kind[ci] and caps.openclose[ci] >= 0 do
        ci, Cf_vi = eval[caps.kind[ci]](caps, sbj, Cf_vals, ci, 1)
        result = func(result, t_unpack(Cf_vals, 1, Cf_vi - 1))
    end
    vals[vi] = result
    return ci +1, vi + 1
end
function eval.Cg (caps, sbj, vals, ci, vi)
    if caps.openclose[ci] > 0 then
        vals[vi] = s_sub(sbj, caps.bounds[ci], caps.openclose[ci] - 1)
        return ci + 1, vi + 1
    end
    local cj, vj = insert(caps, sbj, vals, ci + 1, vi)
    if vj == vi then
        vals[vj] = s_sub(sbj, caps.bounds[ci], caps.bounds[cj] - 1)
        vj = vj + 1
    end
    return cj + 1, vj
end
function eval.Clb (caps, sbj, vals, ci, vi)
    local oc = caps.openclose
    if oc[ci] > 0 then
        return ci + 1, vi
    end
    local depth = 0
    repeat
        if oc[ci] == 0 then depth = depth + 1
        elseif oc[ci] < 0 then depth = depth - 1
        end
        ci = ci + 1
    until depth == 0
    return ci, vi
end
function eval.Cp (caps, sbj, vals, ci, vi)
    vals[vi] = caps.bounds[ci]
    return ci + 1, vi + 1
end
function eval.Ct (caps, sbj, vals, ci, vi)
    local aux, openclose, kind = caps. aux, caps.openclose, caps.kind
    local tbl_vals = {}
    vals[vi] = tbl_vals
    if openclose[ci] > 0 then
        return ci + 1, vi + 1
    end
    local tbl_vi, Clb_vals = 1, {}
    ci = ci + 1
    while kind[ci] and openclose[ci] >= 0 do
        if kind[ci] == "Clb" then
            local label, Clb_vi = aux[ci], 1
            ci, Clb_vi = eval.Cg(caps, sbj, Clb_vals, ci, 1)
            if Clb_vi ~= 1 then tbl_vals[label] = Clb_vals[1] end
        else
            ci, tbl_vi =  eval[kind[ci]](caps, sbj, tbl_vals, ci, tbl_vi)
        end
    end
    return ci + 1, vi + 1
end
local inf = 1/0
function eval.value (caps, sbj, vals, ci, vi)
    local val
    if caps.aux[ci] ~= inf or caps.openclose[ci] ~= inf
        then val = caps.aux[ci]
    end
    vals[vi] = val
    return ci + 1, vi + 1
end
function eval.Cs (caps, sbj, vals, ci, vi)
    if caps.openclose[ci] > 0 then
        vals[vi] = s_sub(sbj, caps.bounds[ci], caps.openclose[ci] - 1)
    else
        local bounds, kind, openclose = caps.bounds, caps.kind, caps.openclose
        local start, buffer, Cs_vals, bi, Cs_vi = bounds[ci], {}, {}, 1, 1
        local last
        ci = ci + 1
        while openclose[ci] >= 0 do
            last = bounds[ci]
            buffer[bi] = s_sub(sbj, start, last - 1)
            bi = bi + 1
            ci, Cs_vi = eval[kind[ci]](caps, sbj, Cs_vals, ci, 1)
            if Cs_vi > 1 then
                buffer[bi] = Cs_vals[1]
                bi = bi + 1
                start = openclose[ci-1] > 0 and openclose[ci-1] or bounds[ci-1]
            else
                start = last
            end
        end
        buffer[bi] = s_sub(sbj, start, bounds[ci] - 1)
        vals[vi] = t_concat(buffer)
    end
    return ci + 1, vi + 1
end
local
function insert_divfunc_results(acc, val_i, ...)
    local n = select('#', ...)
    for i = 1, n do
        val_i, acc[val_i] = val_i + 1, select(i, ...)
    end
    return val_i
end
function eval.div_function (caps, sbj, vals, ci, vi)
    local func = caps.aux[ci]
    local params, divF_vi
    if caps.openclose[ci] > 0 then
        params, divF_vi = {s_sub(sbj, caps.bounds[ci], caps.openclose[ci] - 1)}, 2
    else
        params = {}
        ci, divF_vi = insert(caps, sbj, params, ci + 1, 1)
    end
    ci = ci + 1 -- skip the closed or closing node.
    vi = insert_divfunc_results(vals, vi, func(t_unpack(params, 1, divF_vi - 1)))
    return ci, vi
end
function eval.div_number (caps, sbj, vals, ci, vi)
    local this_aux = caps.aux[ci]
    local divN_vals, divN_vi
    if caps.openclose[ci] > 0 then
        divN_vals, divN_vi = {s_sub(sbj, caps.bounds[ci], caps.openclose[ci] - 1)}, 2
    else
        divN_vals = {}
        ci, divN_vi = insert(caps, sbj, divN_vals, ci + 1, 1)
    end
    ci = ci + 1 -- skip the closed or closing node.
    if this_aux >= divN_vi then error("no capture '"..this_aux.."' in /number capture.") end
    vals[vi] = divN_vals[this_aux]
    return ci, vi + 1
end
local function div_str_cap_refs (caps, ci)
    local opcl = caps.openclose
    local refs = {open=caps.bounds[ci]}
    if opcl[ci] > 0 then
        refs.close = opcl[ci]
        return ci + 1, refs, 0
    end
    local first_ci = ci
    local depth = 1
    ci = ci + 1
    repeat
        local oc = opcl[ci]
        if depth == 1  and oc >= 0 then refs[#refs+1] = ci end
        if oc == 0 then
            depth = depth + 1
        elseif oc < 0 then
            depth = depth - 1
        end
        ci = ci + 1
    until depth == 0
    refs.close = caps.bounds[ci - 1]
    return ci, refs, #refs
end
function eval.div_string (caps, sbj, vals, ci, vi)
    local n, refs
    local cached
    local cached, divS_vals = {}, {}
    local the_string = caps.aux[ci]
    ci, refs, n = div_str_cap_refs(caps, ci)
    vals[vi] = the_string:gsub("%%([%d%%])", function (d)
        if d == "%" then return "%" end
        d = tonumber(d)
        if not cached[d] then
            if d > n then
                error("no capture at index "..d.." in /string capture.")
            end
            if d == 0 then
                cached[d] = s_sub(sbj, refs.open, refs.close - 1)
            else
                local _, vi = eval[caps.kind[refs[d]]](caps, sbj, divS_vals, refs[d], 1)
                if vi == 1 then error("no values in capture at index"..d.." in /string capture.") end
                cached[d] = divS_vals[1]
            end
        end
        return cached[d]
    end)
    return ci, vi + 1
end
function eval.div_table (caps, sbj, vals, ci, vi)
    local this_aux = caps.aux[ci]
    local key
    if caps.openclose[ci] > 0 then
        key =  s_sub(sbj, caps.bounds[ci], caps.openclose[ci] - 1)
    else
        local divT_vals, _ = {}
        ci, _ = insert(caps, sbj, divT_vals, ci + 1, 1)
        key = divT_vals[1]
    end
    ci = ci + 1
    if this_aux[key] then
        vals[vi] = this_aux[key]
        return ci, vi + 1
    else
        return ci, vi
    end
end
function LL.evaluate (caps, sbj, ci)
    local vals = {}
    local _,  vi = insert(caps, sbj, vals, ci, 1)
    return vals, 1, vi - 1
end
end  -- Decorator wrapper

end
end
--=============================================================================
do local _ENV = _ENV
packages['printers'] = function (...)
return function(Builder, LL)
local ipairs, pairs, print, tostring, type
    = ipairs, pairs, print, tostring, type
local s, t, u = l_Require"string", l_Require"table", l_Require"util"
local S_tostring = Builder.set.tostring
local _ENV = u.noglobals() ----------------------------------------------------
local s_char, s_sub, t_concat
    = s.char, s.sub, t.concat
local   expose,   load,   map
    = u.expose, u.load, u.map
local escape_index = {
    ["\f"] = "\\f",
    ["\n"] = "\\n",
    ["\r"] = "\\r",
    ["\t"] = "\\t",
    ["\v"] = "\\v",
    ["\127"] = "\\ESC"
}
local function flatten(kind, list)
    if list[2].pkind == kind then
        return list[1], flatten(kind, list[2])
    else
        return list[1], list[2]
    end
end
for i = 0, 8 do escape_index[s_char(i)] = "\\"..i end
for i = 14, 31 do escape_index[s_char(i)] = "\\"..i end
local
function escape( str )
    return str:gsub("%c", escape_index)
end
local
function set_repr (set)
    return s_char(load("return "..S_tostring(set))())
end
local printers = {}
local
function LL_pprint (pt, offset, prefix)
    return printers[pt.pkind](pt, offset, prefix)
end
function LL.pprint (pt0)
    local pt = LL.P(pt0)
    print"\nPrint pattern"
    LL_pprint(pt, "", "")
    print"--- /pprint\n"
    return pt0
end
for k, v in pairs{
    string       = [[ "P( \""..escape(pt.as_is).."\" )"       ]],
    char         = [[ "P( \""..escape(to_char(pt.aux)).."\" )"]],
    ["true"]     = [[ "P( true )"                     ]],
    ["false"]    = [[ "P( false )"                    ]],
    eos          = [[ "~EOS~"                         ]],
    one          = [[ "P( one )"                      ]],
    any          = [[ "P( "..pt.aux.." )"             ]],
    set          = [[ "S( "..'"'..escape(set_repr(pt.aux))..'"'.." )" ]],
    ["function"] = [[ "P( "..pt.aux.." )"             ]],
    ref = [[
        "V( ",
            (type(pt.aux) == "string" and "\""..pt.aux.."\"")
                          or tostring(pt.aux)
        , " )"
        ]],
    range = [[
        "R( ",
            escape(t_concat(map(
                pt.as_is,
                function(e) return '"'..e..'"' end)
            , ", "))
        ," )"
        ]]
} do
    printers[k] = load(([==[
        local k, map, t_concat, to_char, escape, set_repr = ...
        return function (pt, offset, prefix)
            print(t_concat{offset,prefix,XXXX})
        end
    ]==]):gsub("XXXX", v), k.." printer")(k, map, t_concat, s_char, escape, set_repr)
end
for k, v in pairs{
    ["behind"] = [[ LL_pprint(pt.pattern, offset, "B ") ]],
    ["at least"] = [[ LL_pprint(pt.pattern, offset, pt.aux.." ^ ") ]],
    ["at most"] = [[ LL_pprint(pt.pattern, offset, pt.aux.." ^ ") ]],
    unm        = [[LL_pprint(pt.pattern, offset, "- ")]],
    lookahead  = [[LL_pprint(pt.pattern, offset, "# ")]],
    choice = [[
        print(offset..prefix.."+")
        local ch, i = {}, 1
        while pt.pkind == "choice" do
            ch[i], pt, i = pt[1], pt[2], i + 1
        end
        ch[i] = pt
        map(ch, LL_pprint, offset.." :", "")
        ]],
    sequence = [=[
        print(offset..prefix.."*")
        local acc, p2 = {}
        offset = offset .. " |"
        while true do
            if pt.pkind ~= "sequence" then -- last element
                if pt.pkind == "char" then
                    acc[#acc + 1] = pt.aux
                    print(offset..'P( "'..s.char(u.unpack(acc))..'" )')
                else
                    if #acc ~= 0 then
                        print(offset..'P( "'..s.char(u.unpack(acc))..'" )')
                    end
                    LL_pprint(pt, offset, "")
                end
                break
            elseif pt[1].pkind == "char" then
                acc[#acc + 1] = pt[1].aux
            elseif #acc ~= 0 then
                print(offset..'P( "'..s.char(u.unpack(acc))..'" )')
                acc = {}
                LL_pprint(pt[1], offset, "")
            else
                LL_pprint(pt[1], offset, "")
            end
            pt = pt[2]
        end
        ]=],
    grammar   = [[
        print(offset..prefix.."Grammar")
        for k, pt in pairs(pt.aux) do
            local prefix = ( type(k)~="string"
                             and tostring(k)
                             or "\""..k.."\"" )
            LL_pprint(pt, offset.."  ", prefix .. " = ")
        end
    ]]
} do
    printers[k] = load(([[
        local map, LL_pprint, pkind, s, u, flatten = ...
        return function (pt, offset, prefix)
            XXXX
        end
    ]]):gsub("XXXX", v), k.." printer")(map, LL_pprint, type, s, u, flatten)
end
for _, cap in pairs{"C", "Cs", "Ct"} do
    printers[cap] = function (pt, offset, prefix)
        print(offset..prefix..cap)
        LL_pprint(pt.pattern, offset.."  ", "")
    end
end
for _, cap in pairs{"Cg", "Clb", "Cf", "Cmt", "div_number", "/zero", "div_function", "div_table"} do
    printers[cap] = function (pt, offset, prefix)
        print(offset..prefix..cap.." "..tostring(pt.aux or ""))
        LL_pprint(pt.pattern, offset.."  ", "")
    end
end
printers["div_string"] = function (pt, offset, prefix)
    print(offset..prefix..'/string "'..tostring(pt.aux or "")..'"')
    LL_pprint(pt.pattern, offset.."  ", "")
end
for _, cap in pairs{"Carg", "Cp"} do
    printers[cap] = function (pt, offset, prefix)
        print(offset..prefix..cap.."( "..tostring(pt.aux).." )")
    end
end
printers["Cb"] = function (pt, offset, prefix)
    print(offset..prefix.."Cb( \""..pt.aux.."\" )")
end
printers["Cc"] = function (pt, offset, prefix)
    print(offset..prefix.."Cc(" ..t_concat(map(pt.aux, tostring),", ").." )")
end
local cprinters = {}
local padding = "   "
local function padnum(n)
    n = tostring(n)
    n = n .."."..((" "):rep(4 - #n))
    return n
end
local function _cprint(caps, ci, indent, sbj, n)
    local openclose, kind = caps.openclose, caps.kind
    indent = indent or 0
    while kind[ci] and openclose[ci] >= 0 do
        if caps.openclose[ci] > 0 then
            print(t_concat({
                            padnum(n),
                            padding:rep(indent),
                            caps.kind[ci],
                            ": start = ", tostring(caps.bounds[ci]),
                            " finish = ", tostring(caps.openclose[ci]),
                            caps.aux[ci] and " aux = " or "",
                            caps.aux[ci] and (
                                type(caps.aux[ci]) == "string"
                                    and '"'..tostring(caps.aux[ci])..'"'
                                or tostring(caps.aux[ci])
                            ) or "",
                            " \t", s_sub(sbj, caps.bounds[ci], caps.openclose[ci] - 1)
                        }))
            if type(caps.aux[ci]) == "table" then expose(caps.aux[ci]) end
        else
            local kind = caps.kind[ci]
            local start = caps.bounds[ci]
            print(t_concat({
                            padnum(n),
                            padding:rep(indent), kind,
                            ": start = ", start,
                            caps.aux[ci] and " aux = " or "",
                            caps.aux[ci] and (
                                type(caps.aux[ci]) == "string"
                                    and '"'..tostring(caps.aux[ci])..'"'
                                or tostring(caps.aux[ci])
                            ) or ""
                        }))
            ci, n = _cprint(caps, ci + 1, indent + 1, sbj, n + 1)
            print(t_concat({
                            padnum(n),
                            padding:rep(indent),
                            "/", kind,
                            " finish = ", tostring(caps.bounds[ci]),
                            " \t", s_sub(sbj, start, (caps.bounds[ci] or 1) - 1)
                        }))
        end
        n = n + 1
        ci = ci + 1
    end
    return ci, n
end
function LL.cprint (caps, ci, sbj)
    ci = ci or 1
    print"\nCapture Printer:\n================"
    _cprint(caps, ci, 0, sbj, 1)
    print"================\n/Cprinter\n"
end
return { pprint = LL.pprint,cprint = LL.cprint }
end -- module wrapper ---------------------------------------------------------

end
end
--=============================================================================
do local _ENV = _ENV
packages['analyzer'] = function (...)

local u = l_Require"util"
local nop, weakkey = u.nop, u.weakkey
local hasVcache, hasCmtcache , lengthcache
    = weakkey{}, weakkey{},    weakkey{}
return {
    hasV = nop,
    hasCmt = nop,
    length = nop,
    hasCapture = nop
}

end
end
--=============================================================================
do local _ENV = _ENV
packages['locale'] = function (...)

local extend = l_Require"util".extend
local _ENV = l_Require"util".noglobals() ----------------------------------------
return function(Builder, LL) -- Module wrapper {-------------------------------
local R, S = LL.R, LL.S
local locale = {}
locale["cntrl"] = R"\0\31" + "\127"
locale["digit"] = R"09"
locale["lower"] = R"az"
locale["print"] = R" ~" -- 0x20 to 0xee
locale["space"] = S" \f\n\r\t\v" -- \f == form feed (for a printer), \v == vtab
locale["upper"] = R"AZ"
locale["alpha"]  = locale["lower"] + locale["upper"]
locale["alnum"]  = locale["alpha"] + locale["digit"]
locale["graph"]  = locale["print"] - locale["space"]
locale["punct"]  = locale["graph"] - locale["alnum"]
locale["xdigit"] = locale["digit"] + R"af" + R"AF"
function LL.locale (t)
    return extend(t or {}, locale)
end
end -- Module wrapper --------------------------------------------------------}

end
end
--=============================================================================
do local _ENV = _ENV
packages['match'] = function (...)

end
end
--=============================================================================
do local _ENV = _ENV
packages['factorizer'] = function (...)
local ipairs, pairs, print, setmetatable
    = ipairs, pairs, print, setmetatable
local u = l_Require"util"
local   id,   nop,   setify,   weakkey
    = u.id, u.nop, u.setify, u.weakkey
local _ENV = u.noglobals() ----------------------------------------------------
local
function process_booleans(a, b, opts)
    local id, brk = opts.id, opts.brk
    if a == id then return true, b
    elseif b == id then return true, a
    elseif a == brk then return true, brk
    else return false end
end
local unary = setify{
    "unm", "lookahead", "C", "Cf",
    "Cg", "Cs", "Ct", "/zero"
}
local unary_aux = setify{
    "behind", "at least", "at most", "Clb", "Cmt",
    "div_string", "div_number", "div_table", "div_function"
}
local unifiable = setify{"char", "set", "range"}
local hasCmt; hasCmt = setmetatable({}, {__mode = "k", __index = function(self, pt)
    local kind, res = pt.pkind, false
    if kind == "Cmt"
    or kind == "ref"
    then
        res = true
    elseif unary[kind] or unary_aux[kind] then
        res = hasCmt[pt.pattern]
    elseif kind == "choice" or kind == "sequence" then
        res = hasCmt[pt[1]] or hasCmt[pt[2]]
    end
    hasCmt[pt] = res
    return res
end})
return function (Builder, LL) --------------------------------------------------
if Builder.options.factorize == false then
    return {
        choice = nop,
        sequence = nop,
        lookahead = nop,
        unm = nop
    }
end
local constructors, LL_P =  Builder.constructors, LL.P
local truept, falsept
    = constructors.constant.truept
    , constructors.constant.falsept
local --Range, Set,
    S_union
    = --Builder.Range, Builder.set.new,
    Builder.set.union
local mergeable = setify{"char", "set"}
local type2cons = {
    ["/zero"] = "__div",
    ["div_number"] = "__div",
    ["div_string"] = "__div",
    ["div_table"] = "__div",
    ["div_function"] = "__div",
    ["at least"] = "__exp",
    ["at most"] = "__exp",
    ["Clb"] = "Cg",
}
local
function choice (a, b)
    do  -- handle the identity/break properties of true and false.
        local hasbool, res = process_booleans(a, b, { id = falsept, brk = truept })
        if hasbool then return res end
    end
    local ka, kb = a.pkind, b.pkind
    if a == b and not hasCmt[a] then
        return a
    elseif ka == "choice" then -- correct associativity without blowing up the stack
        local acc, i = {}, 1
        while a.pkind == "choice" do
            acc[i], a, i = a[1], a[2], i + 1
        end
        acc[i] = a
        for j = i, 1, -1 do
            b = acc[j] + b
        end
        return b
    elseif mergeable[ka] and mergeable[kb] then
        return constructors.aux("set", S_union(a.aux, b.aux))
    elseif mergeable[ka] and kb == "any" and b.aux == 1
    or     mergeable[kb] and ka == "any" and a.aux == 1 then
        return ka == "any" and a or b
    elseif ka == kb then
        if (unary[ka] or unary_aux[ka]) and ( a.aux == b.aux ) then
            return LL[type2cons[ka] or ka](a.pattern + b.pattern, a.aux)
        elseif ( ka == kb ) and ka == "sequence" then
            if a[1] == b[1]  and not hasCmt[a[1]] then
                return a[1] * (a[2] + b[2])
            end
        end
    end
    return false
end
local
function lookahead (pt)
    return pt
end
local
function sequence(a, b)
    do
        local hasbool, res = process_booleans(a, b, { id = truept, brk = falsept })
        if hasbool then return res end
    end
    local ka, kb = a.pkind, b.pkind
    if ka == "sequence" then -- correct associativity without blowing up the stack
        local acc, i = {}, 1
        while a.pkind == "sequence" do
            acc[i], a, i = a[1], a[2], i + 1
        end
        acc[i] = a
        for j = i, 1, -1 do
            b = acc[j] * b
        end
        return b
    elseif (ka == "one" or ka == "any") and (kb == "one" or kb == "any") then
        return LL_P(a.aux + b.aux)
    end
    return false
end
local
function unm (pt)
    if     pt == truept            then return falsept
    elseif pt == falsept           then return truept
    elseif pt.pkind == "unm"       then return #pt.pattern
    elseif pt.pkind == "lookahead" then return -pt.pattern
    end
end
return {
    choice = choice,
    lookahead = lookahead,
    sequence = sequence,
    unm = unm
}
end

end
end
--=============================================================================
do local _ENV = _ENV
packages['API'] = function (...)

local assert, error, ipairs, pairs, pcall, print
    , require, select, tonumber, tostring, type
    = assert, error, ipairs, pairs, pcall, print
    , l_Require, select, tonumber, tostring, type
local t, u = require"table", require"util"
local _ENV = u.noglobals() ---------------------------------------------------
local t_concat = t.concat
local   checkstring,   copy,   fold,   load,   map_fold,   map_foldr,   setify, t_pack, t_unpack
    = u.checkstring, u.copy, u.fold, u.load, u.map_fold, u.map_foldr, u.setify, u.pack, u.unpack
local
function charset_error(index, charset)
    error("Character at position ".. index + 1
            .." is not a valid "..charset.." one.",
        2)
end
return function(Builder, LL) -- module wrapper -------------------------------
local cs = Builder.charset
local constructors, LL_ispattern
    = Builder.constructors, LL.ispattern
local truept, falsept, Cppt
    = constructors.constant.truept
    , constructors.constant.falsept
    , constructors.constant.Cppt
local    split_int,    validate
    = cs.split_int, cs.validate
local Range, Set, S_union, S_tostring
    = Builder.Range, Builder.set.new
    , Builder.set.union, Builder.set.tostring
local factorize_choice, factorize_lookahead, factorize_sequence, factorize_unm
local
function makechar(c)
    return constructors.aux("char", c)
end
local
function LL_P (...)
    local v, n = (...), select('#', ...)
    if n == 0 then error"bad argument #1 to 'P' (value expected)" end
    local typ = type(v)
    if LL_ispattern(v) then
        return v
    elseif typ == "function" then
        return
            LL.Cmt("", v)
    elseif typ == "string" then
        local success, index = validate(v)
        if not success then
            charset_error(index, cs.name)
        end
        if v == "" then return truept end
        return
            map_foldr(split_int(v), makechar, Builder.sequence)
    elseif typ == "table" then
        local g = copy(v)
        if g[1] == nil then error("grammar has no initial rule") end
        if not LL_ispattern(g[1]) then g[1] = LL.V(g[1]) end
        return
            constructors.none("grammar", g)
    elseif typ == "boolean" then
        return v and truept or falsept
    elseif typ == "number" then
        if v == 0 then
            return truept
        elseif v > 0 then
            return
                constructors.aux("any", v)
        else
            return
                - constructors.aux("any", -v)
        end
    else
        error("bad argument #1 to 'P' (lpeg-pattern expected, got "..typ..")")
    end
end
LL.P = LL_P
local
function LL_S (set)
    if set == "" then
        return
            falsept
    else
        local success
        set = checkstring(set, "S")
        return
            constructors.aux("set", Set(split_int(set)), set)
    end
end
LL.S = LL_S
local
function LL_R (...)
    if select('#', ...) == 0 then
        return LL_P(false)
    else
        local range = Range(1,0)--Set("")
        for _, r in ipairs{...} do
            r = checkstring(r, "R")
            assert(#r == 2, "bad argument #1 to 'R' (range must have two characters)")
            range = S_union ( range, Range(t_unpack(split_int(r))) )
        end
        return
            constructors.aux("set", range)
    end
end
LL.R = LL_R
local
function LL_V (name)
    assert(name ~= nil)
    return
        constructors.aux("ref",  name)
end
LL.V = LL_V
do
    local one = setify{"set", "range", "one", "char"}
    local zero = setify{"true", "false", "lookahead", "unm"}
    local forbidden = setify{
        "Carg", "Cb", "C", "Cf",
        "Cg", "Cs", "Ct", "/zero",
        "Clb", "Cmt", "Cc", "Cp",
        "div_string", "div_number", "div_table", "div_function",
        "at least", "at most", "behind"
    }
    local function fixedlen(pt, gram, cycle)
        local typ = pt.pkind
        if forbidden[typ] then return false
        elseif one[typ]  then return 1
        elseif zero[typ] then return 0
        elseif typ == "string" then return #pt.as_is
        elseif typ == "any" then return pt.aux
        elseif typ == "choice" then
            local l1, l2 = fixedlen(pt[1], gram, cycle), fixedlen(pt[2], gram, cycle)
            return (l1 == l2) and l1
        elseif typ == "sequence" then
            local l1, l2 = fixedlen(pt[1], gram, cycle), fixedlen(pt[2], gram, cycle)
            return l1 and l2 and l1 + l2
        elseif typ == "grammar" then
            if pt.aux[1].pkind == "ref" then
                return fixedlen(pt.aux[pt.aux[1].aux], pt.aux, {})
            else
                return fixedlen(pt.aux[1], pt.aux, {})
            end
        elseif typ == "ref" then
            if cycle[pt] then return false end
            cycle[pt] = true
            return fixedlen(gram[pt.aux], gram, cycle)
        else
            print(typ,"is not handled by fixedlen()")
        end
    end
    function LL.B (pt)
        pt = LL_P(pt)
        local len = fixedlen(pt)
        assert(len, "A 'behind' pattern takes a fixed length pattern as argument.")
        if len >= 260 then error("Subpattern too long in 'behind' pattern constructor.") end
        return
            constructors.both("behind", pt, len)
    end
end
local function nameify(a, b)
    return tostring(a)..tostring(b)
end
local
function choice (a, b)
    local name = tostring(a)..tostring(b)
    local ch = Builder.ptcache.choice[name]
    if not ch then
        ch = factorize_choice(a, b) or constructors.binary("choice", a, b)
        Builder.ptcache.choice[name] = ch
    end
    return ch
end
function LL.__add (a, b)
    return
        choice(LL_P(a), LL_P(b))
end
local
function sequence (a, b)
    local name = tostring(a)..tostring(b)
    local seq = Builder.ptcache.sequence[name]
    if not seq then
        seq = factorize_sequence(a, b) or constructors.binary("sequence", a, b)
        Builder.ptcache.sequence[name] = seq
    end
    return seq
end
Builder.sequence = sequence
function LL.__mul (a, b)
    return
        sequence(LL_P(a), LL_P(b))
end
local
function LL_lookahead (pt)
    if pt == truept
    or pt == falsept
    or pt.pkind == "unm"
    or pt.pkind == "lookahead"
    then
        return pt
    end
    return
        constructors.subpt("lookahead", pt)
end
LL.__len = LL_lookahead
LL.L = LL_lookahead
local
function LL_unm(pt)
    return
        factorize_unm(pt)
        or constructors.subpt("unm", pt)
end
LL.__unm = LL_unm
local
function LL_sub (a, b)
    a, b = LL_P(a), LL_P(b)
    return LL_unm(b) * a
end
LL.__sub = LL_sub
local
function LL_repeat (pt, n)
    local success
    success, n = pcall(tonumber, n)
    assert(success and type(n) == "number",
        "Invalid type encountered at right side of '^'.")
    return constructors.both(( n < 0 and "at most" or "at least" ), pt, n)
end
LL.__pow = LL_repeat
for _, cap in pairs{"C", "Cs", "Ct"} do
    LL[cap] = function(pt)
        pt = LL_P(pt)
        return
            constructors.subpt(cap, pt)
    end
end
LL["Cb"] = function(aux)
    return
        constructors.aux("Cb", aux)
end
LL["Carg"] = function(aux)
    assert(type(aux)=="number", "Number expected as parameter to Carg capture.")
    assert( 0 < aux and aux <= 200, "Argument out of bounds in Carg capture.")
    return
        constructors.aux("Carg", aux)
end
local
function LL_Cp ()
    return Cppt
end
LL.Cp = LL_Cp
local
function LL_Cc (...)
    return
        constructors.none("Cc", t_pack(...))
end
LL.Cc = LL_Cc
for _, cap in pairs{"Cf", "Cmt"} do
    local msg = "Function expected in "..cap.." capture"
    LL[cap] = function(pt, aux)
    assert(type(aux) == "function", msg)
    pt = LL_P(pt)
    return
        constructors.both(cap, pt, aux)
    end
end
local
function LL_Cg (pt, tag)
    pt = LL_P(pt)
    if tag ~= nil then
        return
            constructors.both("Clb", pt, tag)
    else
        return
            constructors.subpt("Cg", pt)
    end
end
LL.Cg = LL_Cg
local valid_slash_type = setify{"string", "number", "table", "function"}
local
function LL_slash (pt, aux)
    if LL_ispattern(aux) then
        error"The right side of a '/' capture cannot be a pattern."
    elseif not valid_slash_type[type(aux)] then
        error("The right side of a '/' capture must be of type "
            .."string, number, table or function.")
    end
    local name
    if aux == 0 then
        name = "/zero"
    else
        name = "div_"..type(aux)
    end
    return
        constructors.both(name, pt, aux)
end
LL.__div = LL_slash
if Builder.proxymt then
    for k, v in pairs(LL) do
        if k:match"^__" then
            Builder.proxymt[k] = v
        end
    end
else
    LL.__index = LL
end
local factorizer
    = Builder.factorizer(Builder, LL)
factorize_choice,  factorize_lookahead,  factorize_sequence,  factorize_unm =
factorizer.choice, factorizer.lookahead, factorizer.sequence, factorizer.unm
end -- module wrapper --------------------------------------------------------

end
end
--=============================================================================
do local _ENV = _ENV
packages['constructors'] = function (...)

local getmetatable, ipairs, newproxy, print, setmetatable
    = getmetatable, ipairs, newproxy, print, setmetatable
local t, u, compat
    = l_Require"table", l_Require"util", l_Require"compat"
local t_concat = t.concat
local   copy,   getuniqueid,   id,   map
    ,   weakkey,   weakval
    = u.copy, u.getuniqueid, u.id, u.map
    , u.weakkey, u.weakval
local _ENV = u.noglobals() ----------------------------------------------------
local patternwith = {
    constant = {
        "Cp", "true", "false"
    },
    aux = {
        "string", "any",
        "char", "range", "set",
        "ref", "sequence", "choice",
        "Carg", "Cb"
    },
    subpt = {
        "unm", "lookahead", "C", "Cf",
        "Cg", "Cs", "Ct", "/zero"
    },
    both = {
        "behind", "at least", "at most", "Clb", "Cmt",
        "div_string", "div_number", "div_table", "div_function"
    },
    none = "grammar", "Cc"
}
return function(Builder, LL) --- module wrapper.
local S_tostring = Builder.set.tostring
local newpattern, pattmt
if compat.proxies and not compat.lua52_len then
    local proxycache = weakkey{}
    local __index_LL = {__index = LL}
    local baseproxy = newproxy(true)
    pattmt = getmetatable(baseproxy)
    Builder.proxymt = pattmt
    function pattmt:__index(k)
        return proxycache[self][k]
    end
    function pattmt:__newindex(k, v)
        proxycache[self][k] = v
    end
    function LL.getdirect(p) return proxycache[p] end
    function newpattern(cons)
        local pt = newproxy(baseproxy)
        setmetatable(cons, __index_LL)
        proxycache[pt]=cons
        return pt
    end
else
    if LL.warnings and not compat.lua52_len then
        print("Warning: The `__len` metamethod won't work with patterns, "
            .."use `LL.L(pattern)` for lookaheads.")
    end
    pattmt = LL
    function LL.getdirect (p) return p end
    function newpattern(pt)
        return setmetatable(pt,LL)
    end
end
Builder.newpattern = newpattern
local
function LL_ispattern(pt) return getmetatable(pt) == pattmt end
LL.ispattern = LL_ispattern
function LL.type(pt)
    if LL_ispattern(pt) then
        return "pattern"
    else
        return nil
    end
end
local ptcache, meta
local
function resetcache()
    ptcache, meta = {}, weakkey{}
    Builder.ptcache = ptcache
    for _, p in ipairs(patternwith.aux) do
        ptcache[p] = weakval{}
    end
    for _, p in ipairs(patternwith.subpt) do
        ptcache[p] = weakval{}
    end
    for _, p in ipairs(patternwith.both) do
        ptcache[p] = {}
    end
    return ptcache
end
LL.resetptcache = resetcache
resetcache()
local constructors = {}
Builder.constructors = constructors
constructors["constant"] = {
    truept  = newpattern{ pkind = "true" },
    falsept = newpattern{ pkind = "false" },
    Cppt    = newpattern{ pkind = "Cp" }
}
local getauxkey = {
    string = function(aux, as_is) return as_is end,
    table = copy,
    set = function(aux, as_is)
        return S_tostring(aux)
    end,
    range = function(aux, as_is)
        return t_concat(as_is, "|")
    end,
    sequence = function(aux, as_is)
        return t_concat(map(getuniqueid, aux),"|")
    end
}
getauxkey.choice = getauxkey.sequence
constructors["aux"] = function(typ, aux, as_is)
    local cache = ptcache[typ]
    local key = (getauxkey[typ] or id)(aux, as_is)
    if not cache[key] then
        cache[key] = newpattern{
            pkind = typ,
            aux = aux,
            as_is = as_is
        }
    end
    return cache[key]
end
constructors["none"] = function(typ, aux)
    return newpattern{
        pkind = typ,
        aux = aux
    }
end
constructors["subpt"] = function(typ, pt)
    local cache = ptcache[typ]
    if not cache[pt] then
        cache[pt] = newpattern{
            pkind = typ,
            pattern = pt
        }
    end
    return cache[pt]
end
constructors["both"] = function(typ, pt, aux)
    local cache = ptcache[typ][aux]
    if not cache then
        ptcache[typ][aux] = weakval{}
        cache = ptcache[typ][aux]
    end
    if not cache[pt] then
        cache[pt] = newpattern{
            pkind = typ,
            pattern = pt,
            aux = aux,
            cache = cache -- needed to keep the cache as long as the pattern exists.
        }
    end
    return cache[pt]
end
constructors["binary"] = function(typ, a, b)
    return newpattern{
        a, b;
        pkind = typ,
    }
end
end -- module wrapper

end
end
--=============================================================================
do local _ENV = _ENV
packages['init'] = function (...)

local getmetatable, setmetatable, pcall
    = getmetatable, setmetatable, pcall
local u = l_Require"util"
local   copy,   map,   nop, t_unpack
    = u.copy, u.map, u.nop, u.unpack
local API, charsets, compiler, constructors
    , datastructures, evaluator, factorizer
    , locale, printers, re
    = t_unpack(map(l_Require,
    { "API", "charsets", "compiler", "constructors"
    , "datastructures", "evaluator", "factorizer"
    , "locale", "printers", "re" }))
local _, package = pcall(l_Require, "package")
local _ENV = u.noglobals() ----------------------------------------------------
local VERSION = "0.12"
local LuVERSION = "0.1.0"
local function global(self, env) setmetatable(env,{__index = self}) end
local function register(self, env)
    pcall(function()
        package.loaded.lpeg = self
        package.loaded.re = self.re
    end)
    if env then
        env.lpeg, env.re = self, self.re
    end
    return self
end
local
function LuLPeg(options)
    options = options and copy(options) or {}
    local Builder, LL
        = { options = options, factorizer = factorizer }
        , { new = LuLPeg
          , version = function () return VERSION end
          , luversion = function () return LuVERSION end
          , setmaxstack = nop --Just a stub, for compatibility.
          }
    LL.util = u
    LL.global = global
    LL.register = register
    ;-- Decorate the LuLPeg object.
    charsets(Builder, LL)
    datastructures(Builder, LL)
    printers(Builder, LL)
    constructors(Builder, LL)
    API(Builder, LL)
    evaluator(Builder, LL)
    ;(options.compiler or compiler)(Builder, LL)
    locale(Builder, LL)
    LL.re = re(Builder, LL)
    return LL
end -- LuLPeg
local LL = LuLPeg()
return LL

end
end
--=============================================================================
do local _ENV = _ENV
packages['compat'] = function (...)

local _, debug, jit
_, debug = pcall(l_Require, "debug")
_, jit = pcall(l_Require, "jit")
jit = _ and jit
local compat = {
    debug = debug,
    lua51 = (_VERSION == "Lua 5.1") and not jit,
    lua52 = _VERSION == "Lua 5.2",
    luajit = jit and true or false,
    jit = jit and jit.status(),
    lua52_len = not #setmetatable({},{__len = function()end}),
    proxies = pcall(function()
        local prox = newproxy(true)
        local prox2 = newproxy(prox)
        assert (type(getmetatable(prox)) == "table"
                and (getmetatable(prox)) == (getmetatable(prox2)))
    end),
    _goto = not not(loadstring or load)"::R::"
}
return compat

end
end
--=============================================================================
do local _ENV = _ENV
packages['optimizer'] = function (...)
-- Nothing for now.
end
end
return l_Require"init"



--                   The Romantic WTF public license.
--                   --------------------------------
--                   a.k.a. version "<3" or simply v3
--
--
--            Dear user,
--
--            The LuLPeg library
--
--                                             \
--                                              '.,__
--                                           \  /
--                                            '/,__
--                                            /
--                                           /
--                                          /
--                       has been          / released
--                  ~ ~ ~ ~ ~ ~ ~ ~       ~ ~ ~ ~ ~ ~ ~ ~
--                under  the  Romantic   WTF Public License.
--               ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~`,´ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
--               I hereby grant you an irrevocable license to
--                ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
--                  do what the gentle caress you want to
--                       ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
--                           with   this   lovely
--                              ~ ~ ~ ~ ~ ~ ~ ~
--                               / library...
--                              /  ~ ~ ~ ~
--                             /    Love,
--                        #   /      ','
--                        #######    ·
--                        #####
--                        ###
--                        #
--
--               -- Pierre-Yves
--
--
--
--            P.S.: Even though I poured my heart into this work,
--                  I _cannot_ provide any warranty regarding
--                  its fitness for _any_ purpose. You
--                  acknowledge that I will not be held liable
--                  for any damage its use could incur.
--
-- -----------------------------------------------------------------------------
--
-- LuLPeg, Copyright (C) 2013 Pierre-Yves Gérardy.
--
-- The `re` module and lpeg.*.*.test.lua,
-- Copyright (C) 2013 Lua.org, PUC-Rio.
--
-- Permission is hereby granted, free of charge,
-- to any person obtaining a copy of this software and
-- associated documentation files (the "Software"),
-- to deal in the Software without restriction,
-- including without limitation the rights to use,
-- copy, modify, merge, publish, distribute, sublicense,
-- and/or sell copies of the Software,
-- and to permit persons to whom the Software is
-- furnished to do so,
-- subject to the following conditions:
--
-- The above copyright notice and this permission notice
-- shall be included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED,
-- INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
-- DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.
end)

return __bundle_require("__root")
