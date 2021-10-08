-- Bundled by luabundle {"luaVersion":"LuaJIT","version":"1.6.0"}
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
--[[
	lovely-imgui V. 1.0 (beta)
	Author: Neer (https://github.com/YoungNeer/lovely-imgui)
]]

local imgui=require("Core")

imgui.label=require("Widgets.Label")
imgui.button=require("Widgets.Button")
imgui.tooltip=require("Widgets.Tooltip")
imgui.checkBox=require("Widgets.CheckBox")
imgui.radioButton=require("Widgets.RadioButton")
imgui.progressBar=require("Widgets.ProgressBar")
imgui.image=require("Widgets.Image")
imgui.imageButton=require("Widgets.ImageButton")
imgui.scaler=require("Widgets.Scaler")
imgui.slider=require("Widgets.Slider")
imgui.stepper=require("Widgets.Stepper")
imgui.textEntry=require("Widgets.TextEntry")
imgui.canvas=imgui.image --you can also draw canvas!
imgui.beginMenuBar=require("Widgets.MenuBar")[1]
imgui.endMenuBar=require("Widgets.MenuBar")[2]
imgui.beginMenu=require("Widgets.Menu")[1]
imgui.endMenu=require("Widgets.Menu")[2]

-- assetLoader=require('Core.assetLoader')
-- require('Themes.Light')

-- love.draw = love.draw or imgui.draw
-- love.update = love.update or imgui.update
-- love.mousepressed = love.mousepressed or imgui.mousepressed
-- love.keypressed = love.keypressed or imgui.keypressed
-- love.mousereleased = love.mousereleased or imgui.mousereleased
-- love.keyreleased = love.keyreleased or imgui.keyreleased
-- love.mousemoved = love.mousemoved or imgui.mousemoved
-- love.resize = love.resize or imgui.resize
-- love.textinput = love.textinput or imgui.textinput

return imgui

end)
__bundle_register("Widgets.Menu", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	Menus generally don't need a menubar for them to rendered
	making them context-menus
]]

local uiState=require("Core.UIState")
local util=require("Core.util")
local theme=require("Core.theme")
local core=require("Core")
local DrawCommands=require("Core.drawCommands")
local Window=require("Widgets.Window")

--Text of the menu and width of all the previous menus
local width --why waste memory in stacks when a single variable can do the job!
local menuText --same case here

local function BeginMenu(text)
	if core.idle then return end
	menuText=text
	width=Window.getMenuWidth()
	local w,h=theme.getFontSize('menu',text..'a')--trick for padding!
	Window.addMenu(w) 
	local x,y=Window.getTopLeft() x=x+width
	if util.mouseOver(x,y,w,h) then
		DrawCommands.registerCommand(-1,function()
			theme.drawMenuHover(text,x,y,w,h)
		end)
	else
		DrawCommands.registerCommand(-1,function()
			theme.drawMenuNormal(text,x,y)
		end)
	end
	return true
end

local function EndMenu()
	local text=menuText --IMPORTANT!!! This is how Lua works dude!
	
end

return {BeginMenu,EndMenu}

--[[
	WORK IN PROGRESS: Wanna help me out :>
]]
end)
__bundle_register("Widgets.Window", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	Even if a program doesn't have any widget it'd still have
	a window!!
	Window can refer to the program window or the user-created
	window (also called frame!)
]]


--The first element in the stack will always be root window!
local stack={
	--The Window will house all the widgets
	{x=0,y=0,widgets={}}
}
stack[1].width,stack[1].height=love.graphics.getDimensions()

local Window={stack=stack}

--Add menu-bar to the window
Window.addMenuBar=function()
	stack[#stack].menuw=5 --think of this as padding!
end

--Add menus to the window
Window.addMenu=function(w)
	-- table.insert(stack[#stack].menu)
	stack[#stack].menuw=stack[#stack].menuw+w
end

--Get the width of all the menus (if that makes sense)
Window.getMenuWidth=function()
	return stack[#stack].menuw
end

--Get the top-left most point (origin) of the window!
Window.getTopLeft=function()
	return stack[#stack].x,stack[#stack].y
end

--Get the dimensions of the window!
Window.getDimensions=function()
	return stack[#stack].width,stack[#stack].height
end

--Gets the bottom-right most point of the window
Window.getBottomRight=function()
	local w,h=Window.getDimensions()
	return stack[#stack].x+w,stack[#stack].y+h
end

--Gets the center of the window!
Window.getCenter=function()
	local w,h=Window.getDimensions()
	return w/2,h/2
end

Window.getOrigin=Window.getTopLeft
Window.getCenterX=Window.getCenter
Window.getCenterY=function() return select(2,Window.getCenter()) end
Window.getTop=function() return select(2,Window.getTopLeft()) end
Window.getLeft=Window.getTopLeft
Window.getBottom=function() return select(2,Window.getBottomRight()) end
Window.getRight=Window.getBottomRight


return Window
end)
__bundle_register("Core.drawCommands", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	We don't need to draw at every frame! That'd be stupid
	So to make the library "bloat-free", imgui draws to a
	canvas which only updates when an input happens!
	Now using a canvas which may result in loss of quality
	so to turn off this feature simply say `imgui.setCanvas(false)`
	anywhere before `imgui.draw()`!
	To explicity refresh canvas say - `imgui.refresh()`
]]

--Command Format: 
--	function() theme.drawButtonNormal(t,x,y,w,h) end
local commands={}

local drawCommands={
	reverseTop=0, --needed for handling Z-index
	canvas=love.graphics.newCanvas(love.graphics.getDimensions()),
	directDraw=false,
	needsRefresh=true, --refresh canvas?
	autoRefresh=true,  --automatically refresh canvas?
	commands=commands
}

local setCanvas=love.graphics.setCanvas

drawCommands.draw=function()
	if drawCommands.directDraw or
	  (drawCommands.autoRefresh and drawCommands.needsRefresh) then
		-- print('drawing')
		if drawCommands.needsRefresh and not drawCommands.directDraw then
			-- print('setting canvas')
			setCanvas(drawCommands.canvas)
			love.graphics.clear()
		end
		--Z-index is automatically handled before drawing!
		drawCommands.handleDepth()
		for i=1,#commands do
			commands[i]()
		end
		if drawCommands.needsRefresh and not drawCommands.directDraw then
			drawCommands.needsRefresh=false
			drawCommands.clear()  --> TODO: Check if this is buggy
			setCanvas()
		end
	end
	if not drawCommands.directDraw then
		love.graphics.setColor(1,1,1)
		love.graphics.draw(drawCommands.canvas)
	end
	drawCommands.reverseTop=0
end

drawCommands.registerCommand=function (zindex,func)
	if not func then func,zindex=zindex,#commands+1 end

	if zindex==-1 then
		--We want this widget to have high depth!
		drawCommands.reverseTop=drawCommands.reverseTop-1
		commands[drawCommands.reverseTop]=func
	else
		table.insert(commands,zindex,func)
	end
	drawCommands.needsRefresh=true
end

--Move all the negative values to positive values!
drawCommands.handleDepth=function()
	local i=-1
	while true do
		if not commands[i] then return end
		table.insert(commands,commands[i])
		commands[i]=nil
		i=i-1
	end
end

drawCommands.clear=function()
	if #commands>100 then
		commands={}
	else
		for i=1,#commands do table.remove(commands,i) end
	end
end

return drawCommands
end)
__bundle_register("Core", function(require, _LOADED, __bundle_register, __bundle_modules)

local imgui=require("Core.base")

function imgui.init()
	love.keyboard.setKeyRepeat(true)
end

function imgui.isMouseReleased()
	return imgui.uiState.mouseUp
end

function imgui.getNextID()
	--this will get the ID of the next widget!
	return imgui['_genID']+1
end

return imgui
end)
__bundle_register("Core.base", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	This module defines the very basic aspects of imgui!
]]

local uiState=require("Core.UIState")
local theme=require("Core.theme")
local util=require("Core.util")
local drawCommands=require("Core.drawCommands")
local Window=require("Widgets.Window")
local Layout=require("Widgets.Layout")


local imgui={
	['_genID']=0,
	['idle']=false,  --don't process any widget if idle!
	['autoIdle']=true,  --make imgui auto-idle for performance optimization
	['overSmart']=true, --don't hover any widget when another widget is already active!
	['pianoMode']=nil, --you'll know it when you see it
	['stack']=stack,
	['layout']=Layout,
	['theme']=theme,
	['uiState']=uiState,
	['drawCommands']=drawCommands
}

function imgui.genID()
	imgui['_genID']=imgui['_genID']+1
	return imgui['_genID']
end

--Performance Optimizations may still be buggy; so use this
function imgui.noOptimize()
	imgui.setCanvas(nil)
	imgui.setAutoIdle(nil)
end

--Set imgui over-smart
function imgui.setOverSmart(v)
	imgui.overSmart=v
end

--Refresh the canvas if there is any canvas
function imgui.refresh()
	if not drawCommands.directDraw and drawCommands.autoRefresh then
		drawCommands.needsRefresh=true
	end
end

function imgui.setAutoIdle(auto)
	imgui.idle=auto
	imgui.autoIdle=auto
end

--Should imgui draw to canvas or directly!
function imgui.setCanvas(canvas)
	drawCommands.directDraw=not canvas
end

--You'll know it when you see it!!
function imgui.setPianoMode(mode)
	imgui.pianoMode=mode
end

function imgui.draw()
	imgui.drawCommands.draw()
	if imgui.autoIdle then imgui.idle=true end
	if uiState.mouseUp==1 then uiState.mouseUp=nil end
	if uiState.mouseUp==0 then uiState.mouseUp=1 end
	if uiState.mouseUp then uiState.mouseUp=nil end
	-- if uiState.mouseUp then print(uiState.mouseUp) end
end

---At every frame we need to reset hotItem to nil
local function imgui_prepare()
	imgui['_genID']=0
	-- uiState.keyChar=nil
	uiState.hotItem=nil
	if imgui.pianoMode then
		print('doing')
		uiState.activeItem=nil
	end
	Layout.reset()
end

function imgui.update(dt)
	if imgui.idle then return end
	imgui.drawCommands.clear()
	imgui_prepare()
	theme.update(dt)	--> why? answered in theme.lua
	uiState.dt=dt
end

--should we support scankeys?? Who uses scan-keys anyway?
function imgui.keypressed(key) 
	uiState.keyDown=key
	if imgui.autoIdle then imgui.idle=false end
	imgui.refresh()
end

function imgui.keyreleased(key)
	uiState.keyDown=nil
	if imgui.autoIdle then imgui.idle=false end
	imgui.refresh()
end


function imgui.textinput(text)
	uiState.keyChar=text
	if imgui.autoIdle then imgui.idle=false end
	imgui.refresh()
end

function imgui.mousepressed(x,y,btn)
	if btn==1 then
		uiState.mouseDown=true
		if imgui.autoIdle then imgui.idle=false end
		imgui.refresh()
	end
end

function imgui.mousereleased(x,y,btn)
	if btn==1 then
		uiState.mouseUp=true
		uiState.mouseDown=false
		uiState.lastActiveItem=uiState.activeItem
		uiState.activeItem=nil
		if imgui.autoIdle then imgui.idle=false end
		imgui.refresh()
	end
end

function imgui.mousemoved(x,y)
	uiState.mouseX,uiState.mouseY=x,y
	if imgui.autoIdle then imgui.idle=false end
	imgui.refresh()
end

function imgui.wheelmoved(x,y)
	uiState.scrollDX=uiState.scrollDX+x
	uiState.scrollDY=uiState.scrollDY+y
	if imgui.autoIdle then imgui.idle=false end
	imgui.refresh()
end

--[INTERNAL]: An attempt to remove redundant logic
function imgui.updateWidget(id,cond)
	if cond then
		if imgui.overSmart then
			if uiState.activeItem and uiState.activeItem~=id then return end
		end
		uiState.hotItem=id
		if uiState.mouseDown and not uiState.activeItem then
			uiState.activeItem=id
		end
	else
		if not imgui.pianoMode then
			if uiState.activeItem==id then uiState.activeItem=0 end
		end
	end
end


function imgui.resize(w,h)
	imgui.resetCanvas(w,h)
	if imgui.autoIdle then imgui.idle=false end
	imgui.refresh()
end

--There's also imgui.resize!!
local setMode=love.window.setMode
love.window.setMode=function(w,h,...)
	imgui.resetCanvas(w,h)
	setMode(w,h,...)
end

--We need to make a new canvas on resizing
function imgui.resetCanvas(w,h)
	--We'll make a new canvas only for increase in window-size!
	if w>Window.stack[1].width or h>Window.stack[1].height then
		drawCommands.canvas:release()
		drawCommands.canvas=love.graphics.newCanvas(w,h)
	end
	Window.stack[1].width,Window.stack[1].height=w,h
end

return imgui
end)
__bundle_register("Widgets.Layout", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	Layout is not exactly a "widget"! You can't "draw a layout"!
	What Layout does is - it helps in setting the position and dimension of a widget
	relatively! Layout are "row-based" where each row is divided into columns.
	By default there's only on column but you can change that! There's also
	something such as the position of the layout and the padding of the layout!
]]

local Window=require("Widgets.Window")

local Layout={
	flex,         --if layout is flexible
	x=0,y=0;      --position of the layout
	cx=0,cy=0;    --position of the cursor
	rh=0;         --max. height of the current row
	px=4,py=9;    --layout padding! (has no effect on the first widget)
	cols=1;       --widgets are first placed column-wise then layout moves to the next-row!
	col=1;        --the current column! Layout will move to the next row when col>cols
}

--[[
	NOTE 1: An important thing about layouts is that they affect only future widget calls!!
	NOTE 2: There are no "rows"! Number of rows can be infinite (theoretically)!
]]

function Layout.setPadding(px,py)
	Layout.px,Layout.py=px,py
end

function Layout.setFlex(val) Layout.flex=val end

--Modified Copy from util.lua

local function getXFromAlign(align)
	if type(align)=='string' then
		if align=='center' then return Window.getCenterX()
		elseif align=='left' then return Window.getLeft()
		elseif align=='right' then return Window.getRight()
		else
			return tonumber(align)*Window.getDimensions()
		end
	else return align end
end

local function getYFromAlign(align)
	if type(align)=='string' then
		if align=='center' then return Window.getCenterY()
		elseif align=='top' then return Window.getTop()
		elseif align=='bottom' then return Window.getBottom()
		else
			local winW,winH=Window.getDimensions()
			return tonumber(align)*winH
		end
	else return align end
end


--Affects only the current row
function Layout.setPosition(x,y,relative)
	x=getXFromAlign(x) or (relative and 0 or Layout.x)
	y=getYFromAlign(y) or (relative and 0 or Layout.y)
	Layout.x,Layout.y=x+(relative and Layout.x or 0),y+(relative and Layout.y or 0)
end

--Affects all future rows
function Layout.setCursor(x,y,relative)
	x=getXFromAlign(x) or (relative and 0 or Layout.cx)
	y=getYFromAlign(y) or (relative and 0 or Layout.cy)
	Layout.cx,Layout.cy=x+(relative and Layout.cx or 0),y+(relative and Layout.cy or 0)
	Layout.setPosition(x,y,relative)
end

function Layout.setColumns(cols)
	Layout.cols=cols
end

function Layout.getPosition(w,h)
	local x,y=Layout.x,Layout.y
	if w then
		Layout.x=Layout.x+Layout.px+w
		Layout.col=Layout.col+1
		Layout.rh=math.max(Layout.rh,h)
		if Layout.col>Layout.cols or (Layout.flex and Layout.x+w+Layout.px>Window.getDimensions()) then
			Layout.moveToNextRow(h)
		end
	end
	return x,y
end

function Layout.reset(x,y)
	Layout.setPadding(4,9)
	Layout.setCursor(x or 0, y or 0)
	Layout.cols,Layout.col=1,1
end

--Normally an internal function but can be used externally without any problem! (hopefully)
function Layout.moveToNextRow()
	Layout.x,Layout.col=Layout.cx,1
	Layout.y=Layout.y+Layout.py+Layout.rh
	Layout.rh=0
end

return Layout

end)
__bundle_register("Core.util", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	We need some utility functions such as pointInRect etc!
	So that's the whole purpose of this util.lua!
]]

local util={}
local uiState=require("Core.UIState")
local param=require("Core.param")
local mosaic=require("Core.mosaic")
local theme=require("Core.theme")
local Layout=require("Widgets.Layout")
local Window=require("Widgets.Window")

function util.warp(value,min,max)
	return value>max and min or (value<min and max or value)
end

function util.constrain(value,min,max)
	return math.min(max,math.max(value,min))
end

function util.pointInRect(x,y,w,h, x0,y0)
	return x<=x0 and x0<=x+w and y<=y0 and y0<=y+h
end

function util.mouseOver(x,y,w,h)
	if uiState.mouseX and uiState.mouseY then
		return util.pointInRect(
			x,y,w,h,uiState.mouseX,uiState.mouseY
		)
	end
end

--Takes in arguments and returns true if one of them is nil
function util.argsOverloaded(argn,...)
	if argn~=select('#',...) then
		return true
	end
	for i=1,argn do
		if select(i,...)==nil then
			return true
		end
	end
end

local function getXFromAlign(align,width)
	if type(align)=='string' then
		if align=='center' then return Window.getCenterX()
		elseif align=='left' then return Window.getLeft()+width/2
		elseif align=='right' then return Window.getRight()-width/2
		else
			local winW,winH=Window.getDimensions()
			return tonumber(align)*winW
		end
	else return align end
end

local function getYFromAlign(align,height)
	if type(align)=='string' then
		if align=='center' then return Window.getCenterY()
		elseif align=='top' then return Window.getTop()+height/2
		elseif align=='bottom' then return Window.getBottom()-height/2
		else
			local winW,winH=Window.getDimensions()
			return tonumber(align)*winH
		end
	else return align end
end

local function getFlexibleSize(w,h)
	local winW,winH=Window.getDimensions()
	if type(w)=='string' then w=winW*w end
	if type(h)=='string' then h=winH*h end
	return w,h
end


--[[
	---Overloads for imgui.image---
		#1: img,x,y,w,h
		#2: img,w,h           => x,y calc by Layout
		#3: img               => x,y calc by Layout, w,h=image_size
	img can be an Image or a table!
]]
function util.getImageParams(img,x,y,w,h)
	local color,r
	img=type(img)=='string' and mosaic.loadImage(img) or img

	if type(img)=='table' then
		color=img.color r=img.rotation img=img.image
	end


	if y and not w then
		--User gave us width and height but not x,y (#2)
		w,h=x,y x,y=nil
	elseif not x then
		--Calculate width and height (#3)
		w,h=img:getDimensions()
	end

	w,h=getFlexibleSize(w,h)
	
	if not x then
		x,y=Layout.getPosition(w,h) --Let layout do its job!!
	else
		x,y=getXFromAlign(x,w),getYFromAlign(y,h)
		--Images already have their origin centered in draw function
	end

	return img,color,r,x,y,w,h
end

--[[
	---Overloads for imgui.checkBox---
		#1: isChecked,text,x,y,w,h
		#2: isChecked,x,y,w,h       => text=nil
		#3: isChecked,text,w,h      => x,y calc by Layout
		#4: isChecked,w,h           => text=nil, x,y calc by Layout
		#5: isChecked               => x,y calc by Layout, w,h=16,16
]]

function util.getCheckBoxParams(isChecked,text,x,y,w,h)
	if type(text)=='number' then --#2 or #4
		x,y,w,h,text=text,x,y,w
	end
	if y and not w then  --#3 or #4
		w,h=x,y
		x,y=nil
	elseif not x then  --#5
		w,h=16,16
	end

	w,h=getFlexibleSize(w,h)

	if not x then
		x,y=Layout.getPosition(w+theme.getFontSize('checkbox',text)+10,h) --Let layout do its job!!
	else
		x,y=getXFromAlign(x,w)-w/2,getYFromAlign(y,h)-h/2
		--For Client: CheckBoxes have origin at the center!
	end

	return isChecked,text,x,y,w,h
end

util.getRadioButtonParams=util.getCheckBoxParams

--[[
	---Overloads for imgui.label---
		#1: text,x,y
		#2: text            => x,y calc by Layout
]]

function util.getLabelParams(text,x,y)
	local w,h
	if type(text)=='string' then
		w,h=theme.getFontSize('label',text)
	else
		w,h=text.font:getWidth(text.text),text.font:getHeight()
	end
	if not x then --#2
		x,y=Layout.getPosition(w,h) --Let layout do its job!!
	else --#1
		x,y=getXFromAlign(x,w)-w/2,getYFromAlign(y,h)-h/2
		--For Client: Labels have origin at the center!
	end
	return x,y
end

--[[
	---Overloads for imgui.tooltip---
		#1: text,x,y
		#2: text,x          => y default to mouseY
		#3: text            => x,y default to mouseX,mouseY
]]

function util.getTooltipParams(text,x,y)
	local w,h
	if type(text)=='string' then
		w,h=theme.getFontSize('tooltip',text)
	else
		w,h=text.font:getWidth(text.text),text.font:getHeight()
	end
	x=x and (getXFromAlign(x,w)-w/2) or uiState.mouseX
	y=y and (getYFromAlign(y,h)-h/2) or (uiState.mouseY+h)
	--For Client: Tooltips have origin at the center!
	return x,y
end


--[[
	---Overloads for imgui.button---
		#1: text,x,y,w,h    
		#2: text,w,h        => x,y calc by Layout
		#3: text            => x,y calc by Layout, w,h from Label
]]

function util.getButtonParams(text,x,y,w,h)
	if y and not w then
		--User gave us width and height but not x,y (#2)
		w,h=x,y x,y=nil
	elseif not w then
		--Get width and height from text (#3)
		w,h=theme.getFontSize('button',text)
	end
	w,h=getFlexibleSize(w,h)
	if not x then
		x,y=Layout.getPosition(w,h) --Let layout do its job!!
	else
		x,y=getXFromAlign(x,w)-w/2,getYFromAlign(y,h)-h/2
		--(#1) For Client: Buttons have origin at the center!
	end
	return text,x,y,w,h
end


--[[
	---Overloads for imgui.imageButton---
		#1: image,x,y,w,h    
		#2: image,w,h        => x,y calc by Layout
		#3: image            => x,y calc by Layout, w,h from image!
]]

function util.getImageButtonParams(img,x,y,w,h)
	if y and not w then
		--User gave us width and height but not x,y (#2)
		w,h=x,y x,y=nil
	elseif not w then
		--Get width and height from text (#3)
		w,h=img.image:getDimensions()
	end
	w,h=getFlexibleSize(w,h)
	if not x then
		x,y=Layout.getPosition(w,h) --Let layout do its job!!
	else
		x,y=getXFromAlign(x,w),getYFromAlign(y,h)
		--Images already have their origin centered in draw function
	end
	return x,y,w,h
end

--[[
	---Overloads for imgui.progressBar---
		#1: progress,x,y,w,h    
		#2: progress,w,h        => x,y calc by Layout
		#3: progress,w          => x,y calc by Layout, h default to 25
]]

function util.getProgressBarParams(progress,x,y,w,h)
	if y and not w then
		--User gave us width and height but not x,y (#2)
		w,h=x,y x,y=nil
	elseif x and not y then
		--Get width and set height to default (#3)
		w=x h=25 x=nil
	end
	w,h=getFlexibleSize(w,h)
	if not x then
		x,y=Layout.getPosition(w,h) --Let layout do its job!!
	else
		x,y=getXFromAlign(x,w)-w/2,getYFromAlign(y,h)-h/2
		--For Client: Progress Bars have origins in the center
	end
	return x,y,w,h
end

--[[
	---Overloads for imgui.scaler---
		#1: scaler,x,y,w,h    
		#2: scaler,w,h        => x,y calc by Layout
		#3: scaler,w/h        => x,y calc by Layout, h/w -> auto_calc
]]

function util.getScalerParams(scaler,x,y,w,h)
	if y and not w then
		--User gave us width and height but not x,y (#2)
		w,h=x,y x,y=nil
	elseif x and not y then
		--Calculate width/height from scaler (#3)
		if scaler.orientation=='t-b' or scaler.orientation=='b-t' then w,h=25,x
		else w,h=x,25 end
		x=nil
	end
	w,h=getFlexibleSize(w,h)
	if not x then
		x,y=Layout.getPosition(w,h) --Let layout do its job!!
	else
		x,y=getXFromAlign(x,w)-w/2,getYFromAlign(y,h)-h/2
	end

	scaler.min=scaler.min or 0
	scaler.max=scaler.max or 1
	scaler.value=scaler.value or scaler.min

	return x,y,w,h
end

--[[
	---Overloads for imgui.slider---
		#1: slider,x,y,w,h    
		#2: slider,x,y,w/h    => h/w -> auto_calc
		#2: slider,w,h        => x,y calc by Layout
		#3: slider,w/h        => x,y calc by Layout, h/w -> auto_calc
]]

local THUMB_SIZE=25 --DRY CODING!!! For now I guess it's alright

function util.getSliderParams(slider,x,y,w,h)
	if y and not w then
		--User gave us width and height but not x,y (#2)
		w,h=x,y x,y=nil
	elseif w and not h then
		--Calculate width/height automatically (#2)
		if slider.vertical then w,h=25,w
		else h=25 end
	elseif x and not y then
		--Calculate width/height from scaler (#3)
		if slider.vertical then w,h=25,x
		else w,h=x,25 end
		x=nil
	end
	w,h=getFlexibleSize(w,h)
	if not x then
		x,y=Layout.getPosition(w,h) --Let layout do its job!!
	else
		x,y=getXFromAlign(x,w)-w/2,getYFromAlign(y,h)-h/2
	end

	slider.min=slider.min or 0
	slider.max=slider.max or 1
	slider.value=slider.value or slider.min

	--Normalize the value of the slider!
	local fraction = (slider.value - slider.min) / (slider.max - slider.min)
	local sx,sy,sw,sh=x,y,w,h
	if slider.vertical then
		sy=util.constrain(y+h*fraction-THUMB_SIZE/2,y,y+h-THUMB_SIZE) sh=THUMB_SIZE
	else
		sx=util.constrain(x+w*fraction-THUMB_SIZE/2,x,x+w-THUMB_SIZE) sw=THUMB_SIZE
	end

	return fraction,x,y,w,h,sx,sy,sw,sh
end

--[[
	---Overloads for imgui.textEntry---
		#1: textEntry,x,y,w,h=30    
		#2: textEntry,w,h=30       => x,y calc by Layout
]]

function util.getTextEntryParams(textEntry,x,y,w,h)
	if x and not w then
		--User gave us width and height but not x,y (#2)
		w,h=x,y x,y=nil
	end
	w,h=getFlexibleSize(w,h or 30)
	if not x then
		x,y=Layout.getPosition(w,h) --Let layout do its job!!
	else
		x,y=getXFromAlign(x,w)-w/2,getYFromAlign(y,h)-h/2
	end
	textEntry.text=textEntry.text or ""
	textEntry._cursor=textEntry._cursor or 0
	return x,y,w,h
end


--[[
	---Overloads for imgui.stepper---
		#1: stepper,x,y,w,h=30    
		#2: stepper,w,h=30       => x,y calc by Layout
]]

function util.getStepperParams(stepper,x,y,w,h)
	if x and not w then
		--User gave us width and height but not x,y (#2)
		w,h=x,y x,y=nil
	end
	w,h=getFlexibleSize(w,h or param.getStepperHeight(w))
	if not x then
		x,y=Layout.getPosition(w,h) --Let layout do its job!!
	else
		x,y=getXFromAlign(x,w)-w/2,getYFromAlign(y,h)-h/2
	end
	stepper.list=stepper.list or {""}
	stepper.active=stepper.active or 1
	return x,y,w,h
end

return util
end)
__bundle_register("Core.theme", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	How should a button be rendered when it's hovered, etc?
	All of this done by the (current) theme!
]]

local theme={
	current,
	list={}
}

theme.list['default']=require("Themes.Primitive")

--[[
	Every widget will have an outline that
	depending on theme may or may not get drawn!
]]
local funcs={
	'update','getFontSize','drawButtonNormal','drawButtonHover',
	'drawButtonPressed','drawCheckBoxNormal',
	'drawCheckBoxHover','drawCheckBoxPressed','drawRadioButtonNormal',
	'drawRadioButtonHover','drawRadioButtonPressed','drawScalerNormal',
	'drawScalerHover','drawScalerPressed',
	'drawLabel','drawTooltip','drawProgressBar','drawMenuBar','drawMenuNormal',
	'drawMenuHover','drawSliderNormal','drawSliderHover','drawSliderPressed',
	'drawTextEntryNormal','drawTextEntryHover','drawTextEntryPressed',
	'drawStepperNormal','drawStepperHover','drawStepperPressed',
	'drawOutline',
}

theme.set=function(themeName)
	theme.current=theme.list[themeName]
	--So rendering the theme will simply render the current theme!
	for _,func in ipairs(funcs) do
		theme[func]=theme.current[func]
	end
end

theme.set('default')

--[[
	Why theme.update(dt)???
	You know that themes not just affect appearance but also performance!
	And themes can just change the "look" but also the "feel" which
	makes them so similar to Java's LAF! With update, one can do tweening
	for a widget transition! (button-hover,etc!)

	Why onActivate and onDeactivate?
	So that themes can turn on/off idle-mode, piano-mode and canvas-rendering
	(eg. canvas-rendering is on by default and is good for performance but is
	sometimes bad for appearance hence a theme may disable it and enable it
	when it is switched!!)
]]

return theme
end)
__bundle_register("Themes.Primitive", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	The thing about primitive theme is that it's more efficient!
]]

local param=require("Core.param")

local theme={}
local lg,ef=love.graphics,function() end

theme.update=ef

local nc,hc,ac={.5,.5,.5},{.55,.55,.55},{.45,.45,.45}

--For Primitive theme, all widgets will have same color palette!
theme.normalColor=nc
theme.hotColor=hc
theme.activeColor=ac

-- For Primitive Theme, all widgets will have same font but you can change this if you like!
theme.font=lg.newFont(15)
theme.arrowFont=lg.newFont(20) --for arrows only

--[[
	Widgets can have different fonts but as a rule they must have
	the same font across different states (hovered,clicked,etc)
	getFontSize takes in the type of the widget and the text
	and returns the minimum-size that is required by that text!
	[Themes can be malice and return a greater or lower size!]
]]

function theme.getFontSize(widget,text)
	local w,h=theme.font:getWidth(text),theme.font:getHeight()
	if widget=='button' then
		w,h=w+50,h+30
	end
	return w,h
end

function theme.drawOutline(widget,x,y,w,h)
	if widget=='button' then return end
	lg.setColor(0,.1,.1)
	lg.rectangle('line',x,y,w,h)
end

---------------------HELPER FUNCTIONS-----------------------

local function constrain(value,min,max)
	return math.min(max,math.max(value,min))
end

local function drawButtonText(text,x,y,w,h)
	lg.setColor(.8,.8,.8)
	lg.setFont(theme.font)
	lg.printf(text,x,y+(h-theme.font:getHeight())/2,w,'center')
end

local function drawTick(x,y,w,h)
	lg.setLineStyle('smooth')
	lg.setLineWidth(w/16)
	lg.setLineJoin("bevel")
	lg.line(x+h*.2,y+h*.6, x+h*.45,y+h*.75, x+h*.8,y+h*.2)
	lg.setLineWidth(1)
end

local function drawCheckBoxText(text,x,y,h)
	lg.setColor(.2,.2,.2)
	lg.setFont(theme.font)
	lg.print(text,x,y+(h-theme.font:getHeight())/2)
end

local drawRadioButtonText=drawCheckBoxText

------------------------STEPPER------------------------

function theme.drawStepperNormal(stepper,x,y,w,h)
	local lx,ly,lw,lh=param.getStepperLeftButton(stepper,x,y,w,h)
	local rx,ry,rw,rh=param.getStepperRightButton(stepper,x,y,w,h)
	ly=ly-2 ry=ry-2
	lg.setColor(nc[1]-.05,nc[2]-.05,nc[3]-.05)
	lg.rectangle('fill',lx,ly,lw,lh+6,4,4)
	lg.rectangle('fill',rx,ry,rw,rh+6,4,4)
	lg.setColor(nc[1]+.03,nc[2]+.03,nc[3]+.03)
	lg.rectangle('fill',x,y,w,h,4,4)
	lg.setColor(unpack(nc))
	lg.rectangle('fill',lx,ly,lw,lh+2,4,4)
	lg.rectangle('fill',rx,ry,rw,rh+2,4,4)
	lg.setColor(.2,.2,.2)
	lg.setColor(.8,.8,.8)
	lg.setFont(theme.font)
	lg.printf(stepper.list[stepper.active],x,y+(h-theme.font:getHeight())/2,w,'center')
	lg.setColor(.7,.7,.7)
	lg.setFont(theme.arrowFont)
	lg.printf('>',rx,ry+(rh-theme.font:getHeight())/2,rw,'center')
	lg.printf('<',lx,ly+(lh-theme.font:getHeight())/2,lw,'center')
end

function theme.drawStepperHover(stepper,left_hovered,x,y,w,h)
	local lx,ly,lw,lh=param.getStepperLeftButton(stepper,x,y,w,h)
	local rx,ry,rw,rh=param.getStepperRightButton(stepper,x,y,w,h)
	ly=ly-2 ry=ry-2
	lg.setColor(nc[1]-.05,nc[2]-.05,nc[3]-.05)
	lg.rectangle('fill',lx,ly,lw,lh+6,4,4)
	lg.rectangle('fill',rx,ry,rw,rh+6,4,4)
	lg.setColor(nc[1]+.03,nc[2]+.03,nc[3]+.03)
	lg.rectangle('fill',x,y,w,h,4,4)
	lg.setColor(unpack(nc))
	if left_hovered then lg.setColor(unpack(hc)) end
	lg.rectangle('fill',lx,ly,lw,lh+2,4,4)
	lg.setColor(unpack(nc))
	if not left_hovered then lg.setColor(unpack(hc)) end
	lg.rectangle('fill',rx,ry,rw,rh+2,4,4)
	lg.setColor(.2,.2,.2)
	lg.setColor(.8,.8,.8)
	lg.setFont(theme.font)
	lg.printf(stepper.list[stepper.active],x,y+(h-theme.font:getHeight())/2,w,'center')
	lg.setColor(.7,.7,.7)
	lg.setFont(theme.arrowFont)
	lg.printf('>',rx,ry+(rh-theme.font:getHeight())/2,rw,'center')
	lg.printf('<',lx,ly+(lh-theme.font:getHeight())/2,lw,'center')
end

function theme.drawStepperPressed(stepper,left_pressed,x,y,w,h)
	local lx,ly,lw,lh=param.getStepperLeftButton(stepper,x,y,w,h)
	local rx,ry,rw,rh=param.getStepperRightButton(stepper,x,y,w,h)
	ly=ly-2 ry=ry-2
	lg.setColor(nc[1]-.05,nc[2]-.05,nc[3]-.05)
	lg.rectangle('fill',lx,ly,lw,lh+6,4,4)
	lg.rectangle('fill',rx,ry,rw,rh+6,4,4)
	lg.setColor(nc[1]+.03,nc[2]+.03,nc[3]+.03)
	lg.rectangle('fill',x,y,w,h,4,4)
	lg.setColor(unpack(nc))
	if left_pressed then lg.setColor(unpack(ac)) end
	lg.rectangle('fill',lx,ly,lw,lh+2,4,4)
	lg.setColor(unpack(nc))
	if not left_pressed then lg.setColor(unpack(ac)) end
	lg.rectangle('fill',rx,ry,rw,rh+2,4,4)
	lg.setColor(.2,.2,.2)
	lg.setColor(.8,.8,.8)
	lg.setFont(theme.font)
	lg.printf(stepper.list[stepper.active],x,y+(h-theme.font:getHeight())/2,w,'center')
	lg.setColor(.7,.7,.7)
	lg.setFont(theme.arrowFont)
	lg.printf('>',rx,ry+(rh-theme.font:getHeight())/2,rw,'center')
	lg.printf('<',lx,ly+(lh-theme.font:getHeight())/2,lw,'center')
end

-----------------------TEXT ENTRY----------------------

function theme.drawTextEntryNormal(text,offset,cursor,x,y,w,h)
	lg.setColor(unpack(nc))
	lg.rectangle('fill',x,y,w,h,4,4)
	lg.setFont(theme.font)
	lg.setColor(1,1,1)
	lg.setScissor(x,y,w,h)
	lg.setColor(.8,.8,.8)
	lg.print(text,x+3,y+(h-theme.font:getHeight())/2)
	lg.setScissor()
end

function theme.drawTextEntryHover(text,offset,cursor,x,y,w,h)
	lg.setColor(unpack(hc))
	lg.rectangle('fill',x,y,w,h,4,4)
	lg.setFont(theme.font)
	lg.setColor(1,1,1)
	lg.setScissor(x,y,w,h)
	lg.setColor(.8,.8,.8)
	lg.print(text,x+3,y+(h-theme.font:getHeight())/2)
	lg.setScissor()
end

function theme.drawTextEntryPressed(text,offset,cursor,x,y,w,h)
	lg.setColor(unpack(ac))
	lg.rectangle('fill',x,y,w,h,2,2)
	lg.setFont(theme.font)
	lg.setColor(1,1,1)
	local left=text:sub(1,cursor)
	local fontWidth=math.ceil((theme.font:getWidth(left)-w)/text:len())
	local right=theme.font:getWidth(left)>w and text:sub(fontWidth,cursor) or text
	lg.setScissor(x,y,w,h)
	if love.timer.getTime()%1.5<=1 then
		lg.print('|',x+3+theme.font:getWidth(left),y+(h-theme.font:getHeight())/2)
	end
	lg.setColor(.8,.8,.8)
	lg.print(right,x+3,y+(h-theme.font:getHeight())/2)
	lg.setScissor()
end

-------------------------LABEL-------------------------

function theme.drawLabel(text,x,y)
	lg.setColor(.8,.8,.8)
	lg.setFont(theme.font)
	lg.print(text,x,y)
end

-------------------------TOOLTIP-------------------------

function theme.drawTooltip(text,x,y)
	lg.setColor(0,0,0)
	lg.rectangle('fill',x-4,y-2,theme.font:getWidth(text)+8,theme.font:getHeight()+2)
	lg.setColor(.8,.8,.8)
	lg.setFont(theme.font)
	lg.print(text,x,y)
end

--TODO Ask GFG if I can write a detailed tutorial on ImGUI!

-----------------------MENU BAR------------------------

function theme.drawMenuBar(x,y,w,h)
	lg.setColor(unpack(nc))
	lg.rectangle('fill',x,y,w,h+4)
end

-------------------------MENUS-------------------------

function theme.drawMenuNormal(text,x,y)
	lg.setColor(.8,.8,.8)
	lg.setFont(theme.font)
	lg.print(text,x,y+2)
end

function theme.drawMenuHover(text,x,y,w,h)
	lg.setColor(unpack(hc))
	lg.rectangle('fill',x-5,y,w+1,h+4)
	lg.setColor(.8,.8,.8)
	lg.setFont(theme.font)
	lg.print(text,x,y+2)
end

----------------------PROGRESS BAR----------------------

function theme.drawProgressBar(text,value,x,y,w,h)
	lg.setColor(unpack(nc))
	lg.rectangle('fill', x,y,w,h,2,2)
	w = w * value
	lg.setColor(.3,.3,.5)
	if value>0 then
		lg.rectangle('fill',x,y,w,h,2,2)
	end
end

----------------------SCALER WIDGET---------------------

function theme.drawScalerNormal(fraction,orientation,x,y,w,h)
	lg.setColor(unpack(nc))
	lg.rectangle('fill', x,y,w,h,2,2)

	if orientation=='t-b' or orientation=='b-t' then
		if orientation=='b-t' then y=y+h*(1-fraction) end
		h = h * fraction
	else
		if orientation=='l-r' then x=x+w*(1-fraction) end
		w = w * fraction
	end

	lg.setColor(nc[1]-.1,nc[2]-.1,nc[3]-.1)
	if fraction>0 then
		lg.rectangle('fill',x,y,w,h,2,2)
	end
end

function theme.drawScalerHover(fraction,orientation,x,y,w,h)
	lg.setColor(unpack(hc))
	lg.rectangle('fill', x,y,w,h,2,2)

	if orientation=='t-b' or orientation=='b-t' then
		if orientation=='b-t' then y=y+h*(1-fraction) end
		h = h * fraction
	else
		if orientation=='l-r' then x=x+w*(1-fraction) end
		w = w * fraction
	end
	
	lg.setColor(hc[1]-.1,hc[2]-.1,hc[3]-.1)
	if fraction>0 then
		lg.rectangle('fill',x,y,w,h,2,2)
	end
end

function theme.drawScalerPressed(fraction,orientation,x,y,w,h)
	x=x+1 y=y+1
	lg.setColor(unpack(ac))
	lg.rectangle('fill', x,y,w,h,2,2)

	if orientation=='t-b' or orientation=='b-t' then
		if orientation=='b-t' then y=y+h*(1-fraction) end
		h = h * fraction
	else
		if orientation=='l-r' then x=x+w*(1-fraction) end
		w = w * fraction
	end

	lg.setColor(ac[1]-.1,ac[2]-.1,ac[3]-.1)
	if fraction>0 then
		lg.rectangle('fill',x,y,w,h,2,2)
	end
end

local THUMB_SIZE=25 --> some DRY coding! Doesn't matter much!

----------------------SLIDER WIDGET---------------------

function theme.drawSliderNormal(fraction,vertical,x,y,w,h)
	lg.setColor(unpack(nc))
	lg.rectangle('line', x,y,w,h,2,2)

	if vertical then
		y=constrain(y+h*fraction-THUMB_SIZE/2,y,y+h-THUMB_SIZE)
		h=THUMB_SIZE
	else
		x=constrain(x+w*fraction-THUMB_SIZE/2,x,x+w-THUMB_SIZE)
		w=THUMB_SIZE
	end

	lg.setColor(nc[1]-.1,nc[2]-.1,nc[3]-.1)
	lg.rectangle('fill',x,y,w,h,2,2)
end

function theme.drawSliderHover(fraction,vertical,x,y,w,h)
	lg.setColor(unpack(hc))
	lg.rectangle('line', x,y,w,h,2,2)

	if vertical then
		y=constrain(y+h*fraction-THUMB_SIZE/2,y,y+h-THUMB_SIZE)
		h=THUMB_SIZE
	else
		x=constrain(x+w*fraction-THUMB_SIZE/2,x,x+w-THUMB_SIZE)
		w=THUMB_SIZE
	end

	lg.setColor(hc[1]-.1,hc[2]-.1,hc[3]-.1)
	lg.rectangle('fill',x,y,w,h,2,2)
end

function theme.drawSliderPressed(fraction,vertical,x,y,w,h)
	-- x=x+1 y=y+1
	lg.setColor(unpack(ac))
	lg.rectangle('line', x,y,w,h,2,2)

	if vertical then
		y=constrain(y+h*fraction-THUMB_SIZE/2,y,y+h-THUMB_SIZE)
		h=THUMB_SIZE
	else
		x=constrain(x+w*fraction-THUMB_SIZE/2,x,x+w-THUMB_SIZE)
		w=THUMB_SIZE
	end

	lg.setColor(ac[1]-.1,ac[2]-.1,ac[3]-.1)
	lg.rectangle('fill',x,y,w,h,2,2)
end

-------------------RADIOBUTTON WIDGET----------------------

function theme.drawRadioButtonNormal(isChecked,text,x,y,w,h)
	lg.setColor(nc[1]-.15,nc[2]-.15,nc[3]-.15)
	lg.setLineWidth(3)
	lg.ellipse('line',x+w/2,y+h/2,w/2,h/2,40)
	lg.setLineWidth(1)
	lg.setColor(unpack(nc))
	lg.ellipse('fill',x+w/2,y+h/2,w/2-2,h/2-2)
	if text then drawRadioButtonText(text,x+w+10,y,h) end
	if isChecked then
		lg.setColor(nc[1]-.25,nc[2]-.25,nc[3]-.25)
		lg.ellipse('fill',x+w/2,y+h/2,w/2-math.floor(1+w/5),h/2-math.floor(1+h/5),40)
	end
end

function theme.drawRadioButtonHover(isChecked,text,x,y,w,h)
	lg.setColor(hc[1]-.15,hc[2]-.15,hc[3]-.15)
	lg.setLineWidth(3)
	lg.ellipse('line',x+w/2,y+h/2,w/2,h/2,40)
	lg.setLineWidth(1)
	lg.setColor(unpack(hc))
	lg.ellipse('fill',x+w/2,y+h/2,w/2-2,h/2-2)
	if text then drawRadioButtonText(text,x+w+10,y,h) end
	if isChecked then
		lg.setColor(hc[1]-.25,hc[2]-.25,hc[3]-.25)
		lg.ellipse('fill',x+w/2,y+h/2,w/2-math.floor(1+w/5),h/2-math.floor(1+h/5),40)
	end
end

function theme.drawRadioButtonPressed(isChecked,text,x,y,w,h)
	y=y+1
	lg.setColor(ac[1]-.2,ac[2]-.2,ac[3]-.2)
	lg.setLineWidth(3)
	lg.ellipse('line',x+w/2,y+h/2,w/2,h/2,40)
	lg.setLineWidth(1)
	lg.setColor(ac[1]-.05,ac[2]-.05,ac[3]-.05)
	lg.ellipse('fill',x+w/2,y+h/2,w/2-2,h/2-2)
	if text then drawRadioButtonText(text,x+w+10,y-1,h) end
	if isChecked then
		lg.setColor(ac[1]-.15,ac[2]-.15,ac[3]-.15)
		lg.ellipse('fill',x+w/2,y+h/2,w/2-math.floor(1+w/5),h/2-math.floor(1+h/5),40)
	end
end

-------------------CHECKBOX WIDGET----------------------

function theme.drawCheckBoxNormal(isChecked,text,x,y,w,h)
	lg.setColor(nc[1]-.05,nc[2]-.05,nc[3]-.05)
	lg.rectangle('fill',x,y,w,h+math.floor(1+h/16),2,2)
	lg.setColor(unpack(nc))
	lg.rectangle('fill',x,y,w,h,2,2)
	if text then drawCheckBoxText(text,x+w+10,y,h) end
	if isChecked then
		lg.setColor(.3,.3,.3)
		drawTick(x,y,w,h)
	end
end

function theme.drawCheckBoxHover(isChecked,text,x,y,w,h)
	lg.setColor(hc[1]-.05,hc[2]-.05,hc[3]-.05)
	lg.rectangle('fill',x,y,w,h+math.floor(1+h/16),2,2)
	lg.setColor(unpack(hc))
	lg.rectangle('fill',x,y,w,h,2,2)
	if text then drawCheckBoxText(text,x+w+10,y,h) end
	if isChecked then
		lg.setColor(.3,.3,.3)
		drawTick(x,y,w,h)
	end
end

function theme.drawCheckBoxPressed(isChecked,text,x,y,w,h)
	lg.setColor(ac[1]-.15,ac[2]-.15,ac[3]-.15)
	lg.rectangle('fill',x,y+2,w,h-2+math.floor(1+h/16),2,2)
	lg.setColor(unpack(ac))
	lg.rectangle('fill',x,y+2,w,h-2,2,2)
	if text then drawCheckBoxText(text,x+w+10,y,h) end
	if isChecked then
		lg.setColor(.3,.3,.3)
		drawTick(x,y+2,w,h)
	end
end

-------------------BUTTON WIDGET----------------------

function theme.drawButtonNormal(text,x,y,w,h)
	lg.setColor(nc[1]-.05,nc[2]-.05,nc[3]-.05)
	lg.rectangle('fill',x,y,w,h+6,5,5)
	lg.setColor(unpack(nc))
	lg.rectangle('fill',x,y,w,h,5,5)
	drawButtonText(text,x,y,w,h)
end

function theme.drawButtonHover(text,x,y,w,h)
	lg.setColor(hc[1]-.05,hc[2]-.05,hc[3]-.05)
	lg.rectangle('fill',x,y,w,h+6,5,5)
	lg.setColor(unpack(hc))
	lg.rectangle('fill',x,y,w,h,5,5)
	drawButtonText(text,x,y,w,h)
end

function theme.drawButtonPressed(text,x,y,w,h)
	lg.setColor(ac[1]-.1,ac[2]-.1,ac[3]-.1)
	lg.rectangle('fill',x,y+3,w,h+3,5,5)
	lg.setColor(unpack(ac))
	lg.rectangle('fill',x,y+3,w,h-3,5,5)
	drawButtonText(text,x,y+2,w,h)
end

return theme
end)
__bundle_register("Core.param", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	Since Magic-Numbers are bad we'll take care of them here!
]]

local param={}

function param.getStepperHeight(w) return w*.25 end

function param.getStepperLeftButton(stepper,x,y,w,h)
	return x,y,w*.2,h
end

function param.getStepperRightButton(stepper,x,y,w,h)
	return x+w-w*.2,y,w*.2,h
end

return param
end)
__bundle_register("Core.mosaic", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	[DISCLAIMER: Stolen from Luigi! Credit:Airstruck ]
	
	Mosaic's main job is to cut slices which are then
	used when rendering a widget which is done by it as well!
	This module is also responsible for maintaining a img-cache
]]

local Mosaic={}

local imgCache = {}
local sliceCache = {}

function Mosaic.loadImage (path)
    if not imgCache[path] then
        imgCache[path] = love.graphics.newImage(path)
    end
    return imgCache[path]
end

local makeSlice = love.graphics.newQuad
local drawSlice=love.graphics.draw

function Mosaic.loadSlices (path)
    local slices = sliceCache[path]

    if not slices then
        slices = {}
        sliceCache[path] = slices
        local image = Mosaic.loadImage(path)
        local iw, ih = image:getWidth(), image:getHeight()
        local w, h = math.floor(iw / 3), math.floor(ih / 3)

        slices.image = image
        slices.width = w
        slices.height = h

        slices.topLeft = makeSlice(0, 0, w, h, iw, ih)
        slices.topCenter = makeSlice(w, 0, w, h, iw, ih)
        slices.topRight = makeSlice(iw - w, 0, w, h, iw, ih)
        slices.middleLeft = makeSlice(0, h, w, h, iw, ih)
        slices.middleCenter = makeSlice(w, h, w, h, iw, ih)
        slices.middleRight = makeSlice(iw - w, h, w, h, iw, ih)
        slices.bottomLeft = makeSlice(0, ih - h, w, h, iw, ih)
        slices.bottomCenter = makeSlice(w, ih - h, w, h, iw, ih)
        slices.bottomRight = makeSlice(iw - w, ih - h, w, h, iw, ih)
    end
    return slices
end

function Mosaic.draw(img,slices,x,y,w,h)
    local sw, sh = slices.width, slices.height
    local xs = (w - sw * 2) / sw -- x scale
    local ys = (h - sh * 2) / sh -- y scale

    drawSlice(img,slices.middleCenter, x + sw, y + sh, 0, xs, ys)
    drawSlice(img,slices.topCenter, x + sw, y, 0, xs, 1)
    drawSlice(img,slices.bottomCenter, x + sw, y + h - sh, 0, xs, 1)
    drawSlice(img,slices.middleLeft, x, y + sh, 0, 1, ys)
    drawSlice(img,slices.middleRight, x + w - sw, y + sh, 0, 1, ys)
    drawSlice(img,slices.topLeft, x, y)
    drawSlice(img,slices.topRight, x + w - sw, y)
    drawSlice(img,slices.bottomLeft, x, y + h - sh)
    drawSlice(img,slices.bottomRight, x + w - sw, y + h - sh)
end

return Mosaic
end)
__bundle_register("Core.UIState", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	ImGUI needs window-dimensions, cursor-positon, etc
	So UIState stores all these information!
]]

--Setting default values *just in case*!!

local UIState={
	dt,        --delta-time
	mouseX=-100,
	mouseY=-100,
	keyChar,   --for text input
	scrollDX,
	scrollDY,
	mouseUp,   --meant to be used externally through an interface!
	mouseDown,  --used internally
	hotItem,
	activeItem,
	lastActiveItem,
	winWidth,
	winHeight
}

return UIState
end)
__bundle_register("Widgets.MenuBar", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	MenuBar is a container for Menus!
	Menus generally don't need a menubar for them to rendered
	making them context-menus
]]

local uiState=require("Core.UIState")
local util=require("Core.util")
local theme=require("Core.theme")
local core=require("Core")
local DrawCommands=require("Core.drawCommands")
local Window=require("Widgets.Window")

local function BeginMenuBar()
	Window.addMenuBar()
	return true
end

local function EndMenuBar()
	local x,y=Window.getTopLeft()
	local w,h=theme.getFontSize('menu','')
	w=Window.getDimensions()
	DrawCommands.registerCommand(function()
		theme.drawMenuBar(x,y,w,h)
	end)
end

return {BeginMenuBar,EndMenuBar}

--[[
	WORK IN PROGRESS: Wanna help me out :>
]]
end)
__bundle_register("Widgets.TextEntry", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	A TextEntry is similar to Java Swing TextField - it's single-line
	and when you hit enter it's done!
]]


--[[
	WORK IN PROGRESS: Wanna help me out :>
	TODO: Auto-scrolling for text (_offset here means just that!)
	TODO: Add selection support!
]]


local uiState=require("Core.UIState")
local util=require("Core.util")
local theme=require("Core.theme")
local core=require("Core")
local DrawCommands=require("Core.drawCommands")

local function split(str, pos) return str:sub(1, pos), str:sub(1+pos) end

local function TextEntry(textEntry,x,y,w,h)
	if core.idle then return end
	local id=core.genID()
	x,y,w,h=util.getTextEntryParams(textEntry,x,y,w,h)
	core.updateWidget(id,util.mouseOver(x,y,w,h))
	if uiState.lastActiveItem==id then
		if uiState.keyChar then
			if not textEntry.limit or textEntry.text:len()<textEntry.limit then
				local left,right=split(textEntry.text,textEntry._cursor)
				textEntry.text=left..uiState.keyChar..right
				textEntry._cursor=textEntry._cursor+1
			end
			uiState.keyChar=nil
		else
			if uiState.keyDown=='left' then
				textEntry._cursor=math.max(0,textEntry._cursor-1)
				uiState.keyDown=nil
			elseif uiState.keyDown=='right' then
				textEntry._cursor=math.min(textEntry.text:len(),textEntry._cursor+1)
				uiState.keyDown=nil
			elseif uiState.keyDown=='delete' then
				local left,right=split(textEntry.text,textEntry._cursor)
				textEntry.text=left..(right:sub(2))
				uiState.keyDown=nil
			elseif uiState.keyDown=='backspace' then
				local left,right=split(textEntry.text,textEntry._cursor)
				textEntry.text=left:sub(1,left:len()-1)..right
				uiState.keyDown=nil
			end
		end
		DrawCommands.registerCommand(function()
			theme.drawTextEntryPressed(textEntry.text,textEntry._offset,textEntry._cursor,x,y,w,h)
		end)
	elseif uiState.hotItem==id then
		love.mouse.setCursor(love.mouse.getSystemCursor('ibeam'))
		DrawCommands.registerCommand(function()
			theme.drawTextEntryHover(textEntry.text,textEntry._offset,textEntry._cursor,x,y,w,h)
		end)
	else
		love.mouse.setCursor()
		DrawCommands.registerCommand(function()
			theme.drawTextEntryNormal(textEntry.text,textEntry._offset,textEntry._cursor,x,y,w,h)
		end)
	end
	return uiState.keyDown=='return' or uiState.keyDown=='enter'
end

return TextEntry
end)
__bundle_register("Widgets.Stepper", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	Stepper is basically a label+two buttons that may-be top-and-down
	or left-and-right depending on direction!
]]


--[[
	WORK IN PROGRESS: Wanna help me out :>
]]


local uiState=require("Core.UIState")
local util=require("Core.util")
local param=require("Core.param")
local theme=require("Core.theme")
local core=require("Core")
local DrawCommands=require("Core.drawCommands")

local function Stepper(stepper,x,y,w,h)
	if core.idle then return end
	local id=core.genID()
	x,y,w,h=util.getStepperParams(stepper,x,y,w,h)
	local lx,ly,lw,lh=param.getStepperLeftButton(stepper,x,y,w,h)
	local rx,ry,rw,rh=param.getStepperRightButton(stepper,x,y,w,h)
	core.updateWidget(
		id,
		util.mouseOver(lx,ly,lw,lh) or util.mouseOver(rx,ry,rw,rh)
	)
	local updated
	if core.isMouseReleased() then
		if util.mouseOver(lx,ly,lw,lh) then
			stepper.active=util.warp(stepper.active-1,1,#stepper.list)
			updated=true
		elseif util.mouseOver(rx,ry,rw,rh) then
			stepper.active=util.warp(stepper.active+1,1,#stepper.list)
			updated=true
		end
		
	end
	if uiState.hotItem==id then
		if uiState.activeItem==id then
			DrawCommands.registerCommand(function()
				theme.drawStepperPressed(stepper,util.mouseOver(lx,ly,lw,lh),x,y,w,h)
			end)
		else
			DrawCommands.registerCommand(function()
				theme.drawStepperHover(stepper,util.mouseOver(lx,ly,lw,lh),x,y,w,h)
			end)
		end
	else
		DrawCommands.registerCommand(function()
			theme.drawStepperNormal(stepper,x,y,w,h)
		end)
	end

	return updated
end

return Stepper
end)
__bundle_register("Widgets.Slider", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	A Slider is slightly more complicated variant of slider!
	It has increments and a "thumb" and by default it doesn't have
	a progress bar but themes can go nasty and implement it!
	Unlike Scalers, Sliders do not have to worry about orientation!
]]

local uiState=require("Core.UIState")
local util=require("Core.util")
local theme=require("Core.theme")
local core=require("Core")
local DrawCommands=require("Core.drawCommands")

local function Slider(slider,x,y,w,h)
	if core.idle then return end
	local fraction,sx,sy,sw,sh  --thumb dimenions and position
	local id=core.genID()
	fraction,x,y,w,h,sx,sy,sw,sh=util.getSliderParams(slider,x,y,w,h)
	
	core.pianoMode=not core.pianoMode
	core.updateWidget(id,util.mouseOver(sx,sy,sw,sh)) --remove 's' if you want to disable "thumb only"!
	core.pianoMode=not core.pianoMode

	if uiState.activeItem==id then
		if slider.vertical then
			fraction = math.min(1, math.max(0, (uiState.mouseY - y) / h))
		else
			fraction = math.min(1, math.max(0, (uiState.mouseX - x) / w))
		end
		DrawCommands.registerCommand(function()
			theme.drawSliderPressed(fraction,slider.vertical,x,y,w,h,slider.style)
		end)
		local v = fraction * (slider.max - slider.min) + slider.min
		if v ~= slider.value then
			slider.value = v
			return true
		end
	elseif uiState.hotItem==id then
		DrawCommands.registerCommand(function()
			theme.drawSliderHover(fraction,slider.vertical,x,y,w,h,slider.style)
		end)
	else
		DrawCommands.registerCommand(function()
			theme.drawSliderNormal(fraction,slider.vertical,x,y,w,h,slider.style)
		end)
	end
end

return Slider
end)
__bundle_register("Widgets.Scaler", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	This draws the Scaler at a relative/absolute position!
	A Scaler is different than a slider in that it doesn't have
	a "thumb"! It also doesn't have to worry about slide-increments
	and stuff like that!
	Also a scaler have *orientation* instead of *direction*!!
]]

local uiState=require("Core.UIState")
local util=require("Core.util")
local theme=require("Core.theme")
local core=require("Core")
local DrawCommands=require("Core.drawCommands")

local function Scaler(scaler,x,y,w,h)
	if core.idle then return end
	local id=core.genID()

	x,y,w,h=util.getScalerParams(scaler,x,y,w,h)

	--Normalize the value of the scaler!
	local fraction = (scaler.value - scaler.min) / (scaler.max - scaler.min)
	core.pianoMode=not core.pianoMode
	core.updateWidget(id,util.mouseOver(x,y,w,h))
	core.pianoMode=not core.pianoMode

	if uiState.activeItem==id then
		if scaler.orientation=='t-b' then
			fraction = math.min(1, math.max(0, (uiState.mouseY - y) / h))
		elseif scaler.orientation=='b-t' then
			fraction = math.min(1, math.max(0, (y+h-uiState.mouseY) / h))
		elseif scaler.orientation=='l-r' then
			fraction = math.min(1, math.max(0, (x+w-uiState.mouseX) / w))
		else
			fraction = math.min(1, math.max(0, (uiState.mouseX - x) / w))
		end
		DrawCommands.registerCommand(function()
			theme.drawScalerPressed(fraction,scaler.orientation,x,y,w,h,scaler.style)
		end)
		local v = fraction * (scaler.max - scaler.min) + scaler.min
		if v ~= scaler.value then
			scaler.value = v
			return true
		end
	elseif uiState.hotItem==id then
		DrawCommands.registerCommand(function()
			theme.drawScalerHover(fraction,scaler.orientation,x,y,w,h,scaler.style)
		end)
	else
		DrawCommands.registerCommand(function()
			theme.drawScalerNormal(fraction,scaler.orientation,x,y,w,h,scaler.style)
		end)
	end
end

return Scaler
end)
__bundle_register("Widgets.ImageButton", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	This draws the Image at a relative/absolute position
]]

local uiState=require("Core.UIState")
local util=require("Core.util")
local core=require("Core")
local DrawCommands=require("Core.drawCommands")

local function ImageButton(img,x,y,w,h)
	if core.idle then return end

	local id=core.genID()
	x,y,w,h=util.getImageButtonParams(img,x,y,w,h)

	local imgW,imgH=img.image:getDimensions()
	local cond=util.mouseOver(x-w/2,y-h/2,w,h)
	if cond and img.precise then
		--If mouse is in the bounding-box and precise-mask is on then
		local imageData=img.data
	end
	core.updateWidget(id,cond)
	
	if uiState.hotItem==id then
		if uiState.activeItem==id then
			DrawCommands.registerCommand(function()
				love.graphics.setColor(1,1,1)
				if img.active then love.graphics.setColor(unpack(img.active)) end
				love.graphics.draw(img.image,x,y,img.rotation,w/imgW,h/imgH,imgW/2,imgH/2)
			end)
		else
			DrawCommands.registerCommand(function()
				love.graphics.setColor(1,1,1)
				if img.hover then love.graphics.setColor(unpack(img.hover)) end
				love.graphics.draw(img.image,x,y,img.rotation,w/imgW,h/imgH,imgW/2,imgH/2)
			end)
		end
	else
		DrawCommands.registerCommand(function()
			love.graphics.setColor(1,1,1)
			if img.default then love.graphics.setColor(unpack(img.default)) end
			love.graphics.draw(img.image,x,y,img.rotation,w/imgW,h/imgH,imgW/2,imgH/2)
		end)
	end

	-- love.graphics.rectangle('line',x-w/2,y-h/2,w,h)
	

	DrawCommands.registerCommand(function()
		love.graphics.setColor(1,1,1)

		
	end)

	return (uiState.hotItem==id and uiState.activeItem==id) or (
		uiState.lastActiveItem==id and uiState.mouseUp
	)
end

return ImageButton
end)
__bundle_register("Widgets.Image", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	This draws the Image at a relative/absolute position
]]

local util=require("Core.util")
local core=require("Core")
local DrawCommands=require("Core.drawCommands")

local function Image(img,x,y,w,h)
	if core.idle then return end
	local imgColor,r
	img,imgColor,r,x,y,w,h=util.getImageParams(img,x,y,w,h)

	local imgW,imgH=img:getDimensions()
	DrawCommands.registerCommand(function()
		love.graphics.setColor(1,1,1)
		if imgColor then love.graphics.setColor(unpack(imgColor)) end
		love.graphics.draw(img,x,y,r,w/imgW,h/imgH,imgW/2,imgH/2)
		-- love.graphics.draw(img,x,y,0,1,1,w/2,h/2)
	end)
end

return Image
end)
__bundle_register("Widgets.ProgressBar", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	Progress Bar are very essential for loading screens and stuff like that!
	Because of the way Lovely-imGUI has been designed you can have variety
	of progress-bars with themes! If you want different progress bars
	without messing with themes then you should probably edit this file!
	Orientation of progress bars is again handled by themes which can make use
	of the style parameter that's passed in the last argument!
]]

local uiState=require("Core.UIState")
local theme=require("Core.theme")
local util=require("Core.util")
local core=require("Core")
local DrawCommands=require("Core.drawCommands")

local function ProgressBar(progressBar,x,y,w,h)
	if core.idle then return end
	x,y,w,h=util.getProgressBarParams(progressBar,x,y,w,h)
	progressBar._timer=progressBar._timer or 0
	progressBar._timer=progressBar._timer+uiState.dt
	--By default the progress is linear and lasts for 5 seconds!
	progressBar.update=progressBar.update or function(dt) return dt/5 end
	progressBar.value=progressBar.value or 0
	if progressBar.value<1 then
		local updateValue=progressBar.update(uiState.dt,progressBar._timer)
		progressBar.value=math.min(1,progressBar.value+updateValue)
	else
		progressBar._timer=0
	end
	DrawCommands.registerCommand(function()
		theme.drawProgressBar(progressBar.text,progressBar.value,x,y,w,h,progressBar.style)
	end)
end

return ProgressBar
end)
__bundle_register("Widgets.RadioButton", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	This draws a radio-button (with/without label) at a relative/absolute position
]]

local uiState=require("Core.UIState")
local util=require("Core.util")
local theme=require("Core.theme")
local core=require("Core")
local DrawCommands=require("Core.drawCommands")

local function RadioButton(rbtn,text,x,y,w,h)
	if core.idle then return end
	local id=core.genID()
	--TODO: Remove rbtn from getRadioParams for possible performance optimization!
	rbtn,text,x,y,w,h=util.getRadioButtonParams(rbtn,text,x,y,w,h)
	--For the check-box and the label!!
	core.updateWidget(
		id,
		util.mouseOver(x,y,w,h) or 
		(text and
			util.mouseOver(
				x+w+15,y,theme.getFontSize('radiobutton',text),h
			)
		)
	)
	if uiState.hotItem==id then
		if uiState.activeItem==id then
			DrawCommands.registerCommand(function()
				theme.drawRadioButtonPressed(rbtn.active,text,x,y,w,h)
			end)
		else
			DrawCommands.registerCommand(function()
				theme.drawRadioButtonHover(rbtn.active,text,x,y,w,h)
			end)
		end
	else
		DrawCommands.registerCommand(function()
			theme.drawRadioButtonNormal(rbtn.active,text,x,y,w,h)
		end)
	end
	--TODO: Make other radio buttons return 0 and this return 1
	if uiState.lastActiveItem==id then
		--If the last thing that was clicked was this radio-button
		rbtn.active=true
		if rbtn.group then
			for i=1,#rbtn.group do
				if rbtn.group[i]~=rbtn then
					rbtn.group[i].active=nil
				end
			end
		end
		uiState.lastActiveItem=nil
		return true
	end
end

return RadioButton
end)
__bundle_register("Widgets.CheckBox", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	This draws a checkbox (with/without label) at a relative/absolute position
]]

local uiState=require("Core.UIState")
local util=require("Core.util")
local theme=require("Core.theme")
local core=require("Core")
local DrawCommands=require("Core.drawCommands")

local function CheckBox(isChecked,text,x,y,w,h)
	if core.idle then return end
	local id=core.genID()
	isChecked,text,x,y,w,h=util.getCheckBoxParams(isChecked,text,x,y,w,h)
	--For the check-box and the label!!
	core.updateWidget(
		id,
		util.mouseOver(x,y,w,h) or 
		(text and
			util.mouseOver(
				x+w+15,y,theme.getFontSize('checkbox',text),h
			)
		)
	)
	if uiState.hotItem==id then
		if uiState.activeItem==id then
			DrawCommands.registerCommand(function()
				theme.drawCheckBoxPressed(isChecked[1],text,x,y,w,h)
			end)
		else
			DrawCommands.registerCommand(function()
				theme.drawCheckBoxHover(isChecked[1],text,x,y,w,h)
			end)
		end
	else
		DrawCommands.registerCommand(function()
			theme.drawCheckBoxNormal(isChecked[1],text,x,y,w,h)
		end)
	end
	if uiState.lastActiveItem==id then
		--If the last thing that was clicked was this check-box
		isChecked[1]=not isChecked[1]
		uiState.lastActiveItem=nil
		return true
	end
end

return CheckBox
end)
__bundle_register("Widgets.Tooltip", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	This draws the Tooltip at a relative/absolute position
	Tooltip speaks for the next widget that is to be rendered!
	Tooltip will normally be displayed instantly! For delays
	one must rely on the LAF!
]]

local theme=require("Core.theme")
local uiState=require("Core.UIState")
local util=require("Core.util")
local core=require("Core")
local DrawCommands=require("Core.drawCommands")

local function Tooltip(text,x,y)
	if core.idle then return end
	local id=core.genID()
	x,y=util.getTooltipParams(text,x,y)
	DrawCommands.registerCommand(-1,function()
		if uiState.hotItem~=id+1 then return end

		if type(text)=='string' then
			theme.drawTooltip(text,x,y)
		else
			love.graphics.setColor(1,1,1)
			if text.color then love.graphics.setColor(text.color) end
			if text.font then love.graphics.setFont(text.font) end
			love.graphics.print(text.text,x,y)
		end
	end)
end

return Tooltip
end)
__bundle_register("Widgets.Button", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	This draws the button at a relative/absolute position
]]

local uiState=require("Core.UIState")
local util=require("Core.util")
local theme=require("Core.theme")
local core=require("Core")
local DrawCommands=require("Core.drawCommands")

local function Button(text,x,y,w,h)
	if core.idle then return end
	local id=core.genID()
	text,x,y,w,h=util.getButtonParams(text,x,y,w,h)
	core.updateWidget(id,util.mouseOver(x,y,w,h))
	if uiState.hotItem==id and uiState.activeItem==id then
		if uiState.activeItem==id then
			DrawCommands.registerCommand(function()
				theme.drawButtonPressed(text,x,y,w,h)
			end)
		else
			DrawCommands.registerCommand(function()
				theme.drawButtonHover(text,x,y,w,h)
			end)
		end
	else
		DrawCommands.registerCommand(function()
			theme.drawButtonNormal(text,x,y,w,h)
		end)
	end

	return (uiState.hotItem==id and uiState.activeItem==id) or (
		uiState.lastActiveItem==id and uiState.mouseUp
	)
end

return Button
end)
__bundle_register("Widgets.Label", function(require, _LOADED, __bundle_register, __bundle_modules)
--[[
	This draws the label at a relative/absolute position
]]

local theme=require("Core.theme")
local util=require("Core.util")
local core=require("Core")
local DrawCommands=require("Core.drawCommands")

local function Label(text,x,y)
	if core.idle then return end
	x,y=util.getLabelParams(text,x,y)
	DrawCommands.registerCommand(function()
		if type(text)=='string' then
			theme.drawLabel(text,x,y)
		else
			love.graphics.setColor(1,1,1)
			if text.color then love.graphics.setColor(text.color) end
			if text.font then love.graphics.setFont(text.font) end
			love.graphics.print(text.text,x,y)
		end
	end)
end

return Label
end)
return __bundle_require("__root")
