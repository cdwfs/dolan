pico-8 cartridge // http://www.pico-8.com
version 41
__lua__
-- dolan's cadillac
--    picostevemo

-- globals

function clamp(x,low,hi)
 return max(low,min(hi,x))
end

function spr_addr(sid)
 return 512*(sid\16)+4*(sid%16)
end

-- creates an animation from a
-- list of sprites and frame counts.
-- one anim per running instance (all frames & counts will be stored per-instance)
-- a:s() increments the frame counter and returns the sprite to show for the new frame.
-- if fcounts is a number,each sprite is shown for that many frames.
-- if fcounts is omitted, each sprite is shown for one frame.
function anim(sprites,fcounts)
 return {
  sp=sprites,
  fc=fcounts or 1,
  fct=type(fcounts)=="table",
  si=0,
  c=1,
  s=function(_ENV)
   c-=1
   if c==0 then
    si=1+si%#sp
    c=fct and fc[si] or fc
   end
   return sp[si]
  end,
 }
end

-- sprite rotation c/o https://www.lexaloffle.com/bbs/?tid=38548
--97 tokens with scaling and arbitrary size
function rspr(x,y,rot,mx,my,w,flip,scale)
 scale=scale or 1
 w*=scale*4
 local cs, ss = cos(rot)*.125/scale,sin(rot)*.125/scale
 local sx, sy = mx+cs*-w, my+ss*-w
 local hx = flip and -w or w
 local halfw = -w
 for py=y-w, y+w do
  tline(x-hx, py, x+hx, py, sx-ss*halfw, sy+cs*halfw, cs, ss)
  halfw+=1
 end
end

-- sounds
s_click=0
s_select=1
s_cancel=2
s_dope1=3
-- sprite ids
sid_empty=16
sid_invisible=32 -- looks empty, but treated as occupied
sid_gems={5,21,37,53}
sid_rock1=25
sid_rock2a=7
sid_rock2b=8
sid_rock2c=23
sid_rock2d=24
sid_car=129
sid_cloud1=65
sid_cloud2=67
sid_cloud3=68
sid_car_bumper1={145,146,161,162}
sid_car_bumper2={141,142,157,158}
sid_car_debris={170,171,172,186,187,188}
sid_digging={1,33,3,35,11,43,13,45}
sid_running={103,71,71,71,71,71,71,103}
sid_sun=101
sid_dirt_pile=65
sid_walking={67,68}
sid_dolan_head=137
-- rotating sprite map locations
m_wheelx,m_wheely,m_wheelw=2,18,2
m_armx,m_army,m_armw=5,18,2
-- sprite flags
sf_rock=0
-- constants
palt_default=0x0040

-- game modes
-- don't edit these directly;
-- call set_next_mode() instead.
modes={}
game_mode=""
next_mode=""
next_mode_enter_args={}

-- switch to a new game mode.
-- args is passed to the new
-- mode's enter() function.
--
-- the transition takes place
-- on the frame following the
-- one in which this function
-- is called.
function set_next_mode(mode,args)
 next_mode=mode
 next_mode_enter_args=args
end

function _init()
 modes={
  menu={
   enter=menu_enter,
   update=menu_update,
   draw=menu_draw,
  },
  match3={
   enter=match3_enter,
   update=match3_update,
   draw=match3_draw,
  },
  carfall={
   enter=cf_enter,
   update=cf_update,
   draw=cf_draw,
  },
  carbury={
   enter=cb_enter,
   update=cb_update,
   draw=cb_draw,
  },
 }
 game_mode="menu"
 next_mode=game_mode
 next_mode_enter_args=nil
 --printh("****************")
 palt(palt_default) -- orange transparent by default
 cls(0)
 bg_init()
 modes[game_mode].enter()
end

function _update60()
 modes[game_mode].update()
end

function _draw()
 modes[game_mode].draw()
 --print(game_mode,1,1,0)
 if next_mode~=game_mode then
  game_mode=next_mode
  modes[game_mode].enter(next_mode_enter_args)
 end
end
-->8
-- match3 mode
b={}

function match3_enter()
 b={
  mode_timer=0,
  mode_timer_max=30*60,
  w=10,
  h=7,
  bx=-100,
  by=72,
  scrollx=2,
  interactive=false,
  crs_fills={
   0x1107.26e1,0x1107.4678,
   0x1107.8764,0x1107.1e62},
  crs_t=0,
  cx=1,
  cy=1,
  -- use mi=2*dx+dy+3 to index
  --   l=1  u=2  z=3  d=4  r=5
  -- 6 and 7 are tile offsets to
  -- rock nw corner.
  -- 8 is cursor size in pixels
  rock_mdists={
   [sid_rock1]={1,1,0,1,1,0,0,7},
   [sid_rock2a]={1,1,0,2,2,0,0,15},
   [sid_rock2b]={2,1,0,2,1,-1,0,15},
   [sid_rock2c]={1,2,0,1,2,0,-1,15},
   [sid_rock2d]={2,2,0,1,1,-1,-1,15},
  },
  grid={},
  yoffs={},
  pgems={},
  selecting=false,
  settling=false,
  digger_a=anim(sid_digging,4),
  dirtx=0,
  dirty=72,
  dirth=0,
  clampx=function(this,x)
   return max(1,min(this.w,x))
  end,
  clampy=function(this,y)
   return max(1,min(this.h,y))
  end,
  cursor_fill=function(_ENV)
   crs_t = selecting
           and 1+crs_t%4 or 1
   return crs_fills[crs_t]
  end,
 }
 -- populate grid
 for y=1,b.h do
  local row,yoffs={},{}
  for x=1,b.w do
   add(row,rnd(sid_gems))
   add(yoffs,0)
  end
  add(b.grid,row)
  add(b.yoffs,yoffs)
 end
 -- iterate until no matches
 while clear_matches(true)>0 do
  b.settling=false
  b.dirth=0
  for y=1,b.h do
   for x=1,b.w do
    b.yoffs[y][x]=0
    if b.grid[y][x]==sid_empty then
     b.grid[y][x]=rnd(sid_gems)
    end
   end
  end
 end
 -- todo: place rocks
 b.grid[1][3]=sid_rock1
 b.grid[1][6]=sid_rock2a
 b.grid[1][7]=sid_rock2b
 b.grid[2][6]=sid_rock2c
 b.grid[2][7]=sid_rock2d
end

-- returns the number of matches
function clear_matches(skip_fx)
 local mtotal=0
 for y=1,b.h do
  local row=b.grid[y]
  for x=1,b.w do
   local s=row[x]
   if s==sid_empty then goto match_end end
   local n=1 -- find x match
   for mx=x+1,b.w do
    if (row[mx]~=s) break
    n+=1
   end
   if n>=3 then -- x matched
    mtotal+=1
    for mx=x,x+n-1 do
     b.yoffs[y][mx]=-1
    end
   end
   n=1 -- find y match
   for my=y+1,b.h do
    if (b.grid[my][x]~=s) break
    n+=1
   end
   if n>=3 then -- y matched
    mtotal+=1
    for my=y,y+n-1 do
     b.yoffs[my][x]=-1
    end
   end
   ::match_end::
   end
 end
 b.dirth+=mtotal
 if mtotal>0 then
  for y=1,b.h do
   for x=1,b.w do
    if b.yoffs[y][x]==-1 then
     if not skip_fx then
      add(b.pgems,{
       s=b.grid[y][x],
       px=b.bx+8*x,
       py=b.by+8*y,
       vx=-3+rnd(2),
       vy=-10+rnd(4),
      })
     end
     b.grid[y][x]=sid_empty
     b.yoffs[y][x]=0
    end
   end
  end
  if (not skip_fx) sfx(s_dope1,0)
  b.settling=true
 end
 return mtotal
end

function settle_grid()
 if b.settling then
  local scount=0
  for y=1,b.h do
   for x=1,b.w do
    -- if not empty and not moving,
    -- see if we should fall.
    -- todo: handle 2x2 rocks.
    -- they should only fall if
    -- both halves are above
    -- empty cells.
    local g=b.grid[y][x]
    if g==sid_rock2a and
       b.yoffs[y][x]==0 then
     if y+1<b.h and
        b.grid[y+2][x]==sid_empty and
        b.grid[y+2][x+1]==sid_empty then
      b.grid[y+1][x]=sid_rock2a
      b.grid[y+1][x+1]=sid_rock2b
      b.grid[y+2][x]=sid_rock2c
      b.grid[y+2][x+1]=sid_rock2d
      b.grid[y][x]=sid_empty
      b.grid[y][x+1]=sid_empty
      b.yoffs[y+1][x]=7
      b.yoffs[y+1][x+1]=7
      b.yoffs[y+2][x]=7
      b.yoffs[y+2][x+1]=7
      scount+=1
     end
    elseif g~=sid_empty and
           g~=sid_rock2b and
           g~=sid_rock2c and
           g~=sid_rock2d and
           b.yoffs[y][x]==0 then
     if y<b.h and
        b.grid[y+1][x]==sid_empty then
      b.grid[y+1][x]=g
      b.grid[y][x]=sid_empty
      b.yoffs[y+1][x]=7
      scount+=1
     end
    -- if already falling, keep
    -- falling.
    elseif b.yoffs[y][x]>0 then
     b.yoffs[y][x]-=1
     scount+=1
    end
   end
  end
  if scount==0 then
   b.settling=false
   clear_matches()
  end
 end
end

function update_pgems()
 local ing=b.pgems
 local outg={}
 for g in all(ing) do
  g.px+=g.vx
  g.py+=g.vy
  g.vy+=1
  if g.px>-8 and g.py<128 then
   add(outg,g)
  end
 end
 b.pgems=outg
end

function match3_update()
 -- debug temp
 if btnp(❎) then
  b.mode_timer=b.mode_timer_max
 end
 -- update timer/sun
 b.mode_timer+=1
 if b.mode_timer>=b.mode_timer_max then
  set_next_mode("carfall",{
   grid=b.grid,
   w=b.w,
   h=b.h,
   bx=b.bx,
   by=b.by,
   runnerx=b.bx-16+8*b.cx,
   dirtx=0,
   dirty=72,
   dirth=b.dirth,
  })
 end
 local mode_t=b.mode_timer/b.mode_timer_max
 bg.sunx=-20+148*(mode_t)
 bg.suny=16+8*cos(mode_t)
 -- scroll
 if b.bx<24 then
  b.bx=min(24,b.bx+b.scrollx)
 else
  b.interactive=true
  b.scrollx=0
 end
 bg_update(b.scrollx)
 if (not b.interactive) return
 settle_grid()
 update_pgems()
 -- select or cancel selection
 if btnp(🅾️) then
  if not b.selecting
    and not b.settling
    and b.grid[b.cy][b.cx]~=sid_empty then
   sfx(s_select,0)
   b.selecting=true
  elseif b.selecting then
   b.selecting=false
   sfx(s_cancel,0)
  end
 end
 -- look up rock under cursor
 -- (default to rock1)
 local sg=b.grid[b.cy][b.cx]
 local isrock=fget(sg,sf_rock)
 local r=b.rock_mdists[
         isrock and sg
                 or sid_rock1]
 -- move cursor
 local dx,dy=0,0
 if btnp(⬆️) then dy-=1 end
 if btnp(⬇️) then dy+=1 end
 if btnp(⬅️) then dx-=1 end
 if btnp(➡️) then dx+=1 end
 if (dx~=0) dy=0 -- no diagonals!
 local mi=2*dx+dy+3
 local mdist=mi~=0 and r[mi]
 local cx2=b:clampx(b.cx+dx*mdist)
 local cy2=b:clampy(b.cy+dy*mdist)
 if cx2~=b.cx or cy2~=b.cy then
  local move_snd=s_click
  -- swap gems before moving cursor
  if b.selecting then
   b.selecting=false
   if sg==sid_rock1 then
    -- only swap if dest cell is empty
    if b.grid[cy2][cx2]==sid_empty then
     b.grid[b.cy][b.cx]=sid_empty
     b.grid[cy2][cx2]=sg
     b.settling=true
    else
     move_snd=s_cancel
    end
   elseif isrock then -- 2x2 rock
    -- get upper-left block coords
    local bx,by=b.cx+r[6],b.cy+r[7]
    -- get coords for cells to check for empty
    local ex,ey=bx+dx+(dx>0 and 1 or 0),
                by+dy+(dy>0 and 1 or 0)
    local ex2,ey2=ex+abs(dy),
                  ey+abs(dx)
    -- only swap if both dest cells are empty
    if b.grid[ey][ex]==sid_empty and
       b.grid[ey2][ex2]==sid_empty then
     b.grid[by][bx]=sid_empty
     b.grid[by][bx+1]=sid_empty
     b.grid[by+1][bx]=sid_empty
     b.grid[by+1][bx+1]=sid_empty
     bx+=dx
     by+=dy
     b.grid[by][bx]=sid_rock2a
     b.grid[by][bx+1]=sid_rock2b
     b.grid[by+1][bx]=sid_rock2c
     b.grid[by+1][bx+1]=sid_rock2d
     cx2,cy2=b.cx+dx,b.cy+dy -- only move one tile
     b.settling=true
    else
     move_snd=s_cancel
    end
   elseif fget(b.grid[cy2][cx2],sf_rock) then
    move_snd=s_cancel
   else
	   b.grid[b.cy][b.cx]=b.grid[cy2][cx2]
	   b.grid[cy2][cx2]=sg
	   move_snd=nil
	   -- revert if not a match
	   if clear_matches()==0 then
	    b.grid[cy2][cx2]=b.grid[b.cy][b.cx]
	    b.grid[b.cy][b.cx]=sg
	    move_snd=s_cancel
	   end
	  end
  end
  b.cx,b.cy=cx2,cy2
  if (move_snd) sfx(move_snd,0)
 end
end

function match3_draw()
 -- draw bg
 bg_draw()
 -- draw board
 map(2,9,b.bx-8,b.by,12,7)
 local by=b.by
 for y=1,b.h do
  local bx=b.bx
  for x=1,b.w do
   spr(b.grid[y][x],bx,
       by-b.yoffs[y][x])
   bx+=8
  end
  by+=8
 end
 -- draw cursor
 if b.interactive then
  poke(0x5f34,1) -- color.fill mode
  local sg=b.grid[b.cy][b.cx]
  local isrock=fget(sg,sf_rock)
  local r=b.rock_mdists[
          isrock and sg
                  or sid_rock1]
  local cx,cy=b.bx+8*(b.cx+r[6])-8,
              b.by+8*(b.cy+r[7])-8
  rect(cx,cy, cx+r[8],cy+r[8],
       b:cursor_fill())
  poke(0x5f34,0)
  fillp()
 end
 -- draw diggin' dude
 spr(b.digger_a:s(),
     b.bx-16+8*b.cx,56,2,2)
 -- draw dirt pile
 local dsx,dsy=8*(sid_dirt_pile%16),
               8*(sid_dirt_pile\16)
 sspr(dsx,dsy,16,16,b.dirtx,b.dirty,
      16,-b.dirth,false,true)
 -- draw particle gems
 for g in all(b.pgems) do
  spr(g.s,g.px,g.py)
 end
 -- debug
 print("temp: press ❎ to\nforce car to arrive",1,1,0)
end
-->8
-- main menu
mm={}

function menu_enter()
 cls(0)
 mm={
  scrollx=1,
  titlex=32,
  titley=12,
  carx=32,
  cary=47,
  wheel_r=0,
 }
 --music(0,2000)
end

function menu_update()
 mm.carx+=mm.scrollx-1
 mm.titlex+=mm.scrollx-1
 if mm.carx>128 then
  set_next_mode("match3")
 end
 bg_update(mm.scrollx)
 mm.wheel_r=(mm.wheel_r+.1793)%1
 if btnp(🅾️) and mm.scrollx<2 then
  sfx(s_dope1,0)
  mm.scrollx=2
 end
 -- tmp music experiment:
 -- remove loop end from current
 -- music pattern
 if btnp(❎) then
  poke(0x3101,0x42)
 end
end

function menu_draw()
 -- background
 bg_draw()
 -- car
 local cardx=2
 -- add a bit of random wheel bumpiness
 local w1y,w2y=rnd()<0.01 and 1 or 0,
               rnd()<0.01 and 1 or 0
 rspr(mm.carx+11+cardx,
      mm.cary+21-w1y,mm.wheel_r,
      m_wheelx,m_wheely,m_wheelw,
      true,0.75)
 rspr(mm.carx+47+cardx,
      mm.cary+21-w2y,mm.wheel_r+0.17,
      m_wheelx,m_wheely,m_wheelw,
      true,0.75)
 spr(sid_car,mm.carx+cardx,
     mm.cary,8,3)
 -- title
 --?"\^w\^t\f1\^j93dolan's\^j96\+cfcadillac\^j93\+ff\f9dolan's\^j96\+becadillac"
 print("\^w\^tdolan's",
       mm.titlex+1+4,
       mm.titley+1,1)
 print("\^w\^tdolan's",
       mm.titlex+4,
       mm.titley,9)
 print("\^w\^tcadillac",
       mm.titlex+1,
       mm.titley+12+1,1)
 print("\^w\^tcadillac",
       mm.titlex,
       mm.titley+12,9)
 -- menu
 if mm.scrollx<2 then
  print("press 🅾️ to start",30,96,0)
  print("press 🅾️ to start",29,95,7)
 end
 -- debug
end
-->8
-- debug
function vardump(value,depth,key)
 local line_prefix=""
 local spaces=""
 if key~=nil then
  line_prefix="["..key.."] = "
 end
 if depth==nil then
  depth=0
 else
  depth+=1
  for i=1,depth do
   spaces=spaces.." "
  end
 end
 local t=type(value)
 if t=="table" then
  local mtable=getmetatable(value)
  if mtable then
   printh(spaces.."(metatable) ")
   value=mtable
  else
   printh(spaces..line_prefix.."(table) ")
  end
  for k,v in pairs(value) do
   vardump(v,depth,k)
  end
 elseif t=="function" then
  printh(spaces..line_prefix..
         "(function)")
 elseif t=="thread"
     or t=="userdata"
     or value==nil then
  printh(spaces..tostr(value))
 else
  printh(spaces..line_prefix..
         "("..t..") "..
         tostr(value))
 end
end

-- print a range of memory to
-- the console,one byte at a
-- time.
function pb(addr,n)
 n = n or 1
 for i=0,n-1 do
  printh(tostr(addr+i,1)..": 0x"..
         sub(tostr(peek(addr+i),1),5,6))
 end
end

-- overwrite the grid with a
-- specific pattern, to simplify
-- testing
function debug_grid(grid)
 local g,r,p,y=sid_gems[1],
               sid_gems[2],
               sid_gems[3],
               sid_gems[4]
 local a,b,c,d,e,_=sid_rock2a,
                   sid_rock2b,
                   sid_rock2c,
                   sid_rock2d,
                   sid_rock1,
                   sid_empty
 return {
 --[[ blank slate
  {_,_,_,_,_,_,_,_,_,_},
  {_,_,_,_,_,_,_,_,_,_},
  {_,_,_,_,_,_,_,_,_,_},
  {_,_,_,_,_,_,_,_,_,_},
  {_,_,_,_,_,_,_,_,_,_},
  {_,_,_,_,_,_,_,_,_,_},
  {_,_,_,_,_,_,_,_,_,_},
 --]]
 --[[
  -- collision bug
  {_,_,_,_,_,_,_,_,_,g},
  {_,_,_,_,_,_,_,_,_,y},
  {p,_,_,_,_,_,_,_,_,p},
  {y,_,_,_,a,b,_,_,r,r},
  {r,_,_,p,c,d,_,_,y,g},
  {g,_,e,g,y,_,r,_,p,r},
  {g,_,g,y,p,_,p,_,r,y},
--]]
 }
end
-->8
-- background
bg={}

function bg_init()
 bg={
  roadx1=0,
  roadx2=-128,
  cactx1=0,
  cactx2=-128,
  clouds={},
  sunx=-16,
  suny=0,
 }
 for i=1,16 do
  add_cloud(16*i-128)
 end
end

function bg_update(dx)
 dx=dx or 0
 -- update clouds
 for c in all(bg.clouds) do
  c.x+=c.vx+0.25*dx
  if (c.x>=128) c.x-=256
 end
 -- update cacti
 bg.cactx1+=0.5*dx
 if (bg.cactx1>=128) bg.cactx1-=256
 bg.cactx2+=0.5*dx
 if (bg.cactx2>=128) bg.cactx2-=256
 -- update road
 bg.roadx1+=1*dx
 if (bg.roadx1>=128) bg.roadx1-=256
 bg.roadx2+=1*dx
 if (bg.roadx2>=128) bg.roadx2-=256
end

function bg_draw()
 -- sky
 rectfill(0,0,127,72,12)
 -- sun
 palt(0x0010)
 spr(sid_sun,bg.sunx,bg.suny,2,2)
 palt(palt_default)
 -- clouds
 for c in all(bg.clouds) do
  ovalfill(c.x,c.y,c.x+c.w,c.y+c.h,c.col)
 end
 -- cacti
 map(16,7,bg.cactx1,56,16,2)
 map(16,7,bg.cactx2,56,16,2)
 -- road
 map(16,9,bg.roadx1,72,16,1)
 map(16,9,bg.roadx2,72,16,1)
 -- dirt
 rectfill(0,75,127,127,5)
end

function add_cloud(x)
 local bx,by=x+rnd(5)\1,rnd(48)\1
 local vx=0.03+rnd(0.002)
 for i=1,2+rnd(4)\1 do
  add(bg.clouds,{
   x=bx+rnd(3)\1,
   y=by+rnd(3)\1,
   w=10+rnd(5),
   h=2+rnd(4),
   vx=0.02,
   col=i<2 and 6 or 7,
  })
 end
end
-->8
-- car-falling
cf={}

function cf_enter(args)
 cf={
  bx=args.bx,
  by=args.by,
  w=args.w,
  h=args.h,
  grid=args.grid,
  runnerx=args.runnerx,
  dirtx=args.dirtx,
  dirty=args.dirty,
  dirth=args.dirth,
  runnery=56,
  runner_t=1,
  collmasks={},
  carx=128,
  cary=45, -- slightly above road
  carvx=-2,
  carvy=0,
  wheel_r=0,
  gravity=0.1,
  -- collision test points on
  -- car, relative to carx/y
  coll_pts={
   { 0, 8},{ 0,13},{ 0,18},{ 4,22},
   { 6,24},{ 9,26},{14,26},
   {17,21},
   {25,21},
   {33,21},{40,21},
   {42,23},{44,26},{50,26},
   {53,22},
   {58,22},{57, 8},{57,13},{58,19},
  },
  debris={},
 }
 -- construct 16x16 bitmask for
 -- entire screen for collision
 -- testing
 for y=1,16 do
  add(cf.collmasks,0x0000)
 end
 local tx0,ty0=1+cf.bx\8,
               1+cf.by\8
 for ty=1,cf.h do
  local mask=0xffff
  local row=ty+ty0-1 -- 1-based
  for tx=1,cf.w do
   if cf.grid[ty][tx]==sid_empty then
    local bit=tx+tx0-2 -- 0-based
    mask &= ~(1<<bit)
   end
  end
  cf.collmasks[row]=mask
 end
end

function update_debris()
 local inp=cf.debris
 local outp={}
 for p in all(inp) do
  p.px+=p.vx
  p.py+=p.vy
  p.vy+=1
  if p.px>-8 and p.py<128 then
   add(outp,p)
  end
 end
 cf.debris=outp
end

function cf_update()
 -- debug temp: advance to next
 -- mode
 if btnp(❎) then
  set_next_mode("carbury",{
   grid=cf.grid,
   w=cf.w,
   h=cf.h,
   bx=cf.bx,
   by=cf.by,
   dirtx=cf.dirtx,
   dirty=cf.dirty,
   dirth=cf.dirth,
   carx=cf.carx,
   cary=cf.cary,
   wheel_r=cf.wheel_r,
  })
 end
 bg_update()
 update_debris()
 cf.wheel_r=(cf.wheel_r+0.17*cf.carvx)%1
 -- fall + collision checks
 function collides(px,py)
  -- if offscreen, assume
  -- infinite road
  if px<0 or px>127 or
     py<0 or py>127 then
   return py>=72
  end
  local bit,row=px\8,1+py\8
  local m=cf.collmasks[row]
  return m&(1<<bit)~=0
 end
 -- x
 local carx2=cf.carx+cf.carvx
 local xfix=0
 for p in all(cf.coll_pts) do
  -- compute point and tile
  -- relative to gem grid
  local px,py=carx2+p[1],
            cf.cary+p[2]
  if collides(px,py) then
   -- collision
   -- compute how far px can go
   -- before it hits this tile.
   local px0=cf.carx+p[1]
   local pxs=cf.carvx<0
         and (px0&0xfff8)\1
         or  (px0|0x7)\1
   if abs(pxs-px)>abs(xfix) then
    xfix=pxs-px
   end
   -- spawn debris particles
   -- at higher velocities
   if abs(cf.carvx)>0.5 then
    local pcount=1+rnd(4)\1
    for i=1,pcount do
     add(cf.debris,{
      s=rnd(sid_car_debris),
      px=pxs,
      py=py,
      vx=cf.carvx,
      vy=-10+rnd(4),
     })
    end
   end
  end
 end
 if xfix~=0 then
  carx2+=xfix
  -- overwrite bumper sprites
  -- with damaged versions
  -- (only needs to happen on
  -- the first hit, but enh)
  for i,s in pairs(sid_car_bumper2) do
   local src,dst=spr_addr(s),
                 spr_addr(sid_car_bumper1[i])
   for y=0,7 do
    memcpy(dst,src,4)
    src+=16*4
    dst+=16*4
   end
  end
  -- flip velocity
  -- todo: clamp small vx to zero
  --       and advance.
  cf.carvx*=-0.5
  if abs(cf.carvx)<0.25 then
   cf.carvx=0
  end
 end
 cf.carx=carx2
 -- y
 -- terminal velocity of 8 to avoid
 -- tunneling through an entire tile
 cf.carvy=min(8,cf.carvy+cf.gravity)
 local cary2=cf.cary+cf.carvy
 local yfix=0
 for p in all(cf.coll_pts) do
  -- compute point and tile
  -- relative to gem grid
  local px,py=cf.carx+p[1],
                cary2+p[2]
  if collides(px,py) then
   -- collision
   -- compute how far py can go
   -- before it hits this tile.
   local py0=cf.cary+p[2]
   local pys=(py0|0x7)\1
   if abs(pys-py)>abs(yfix) then
    yfix=pys-py
   end
  end
 end
 if yfix~=0 then
  cary2+=yfix
  -- zero velocity
  cf.carvy=0
 end
 cf.cary=cary2
 -- animate runner
 cf.runner_t=1+cf.runner_t%8
 cf.runnerx-=2
end

function cf_draw()
 -- draw bg
 bg_draw()
 -- draw board
 map(2,9,cf.bx-8,cf.by,12,7)
 local by=cf.by
 for y=1,cf.h do
  local bx=cf.bx
  for x=1,cf.w do
   spr(cf.grid[y][x],bx,by)
   bx+=8
  end
  by+=8
 end
 -- draw car
 rspr(cf.carx+11,
      cf.cary+21,cf.wheel_r,
      m_wheelx,m_wheely,m_wheelw,
      true,0.75)
 rspr(cf.carx+47,
      cf.cary+21,cf.wheel_r+0.17,
      m_wheelx,m_wheely,m_wheelw,
      true,0.75)
 spr(sid_car,cf.carx,
     cf.cary,8,3)
 -- draw running digger
 spr(sid_running[cf.runner_t],
     cf.runnerx,cf.runnery-5*abs(sin(cf.runner_t/16)),2,2)
 -- draw dirt pile
 local dsx,dsy=8*(sid_dirt_pile%16),
               8*(sid_dirt_pile\16)
 sspr(dsx,dsy,16,16,cf.dirtx,cf.dirty,
      16,-cf.dirth,false,true)
 -- draw debris particles
 for p in all(cf.debris) do
  spr(p.s,p.px,p.py)
 end
 -- debug
 print("temp: press ❎ to\nadvance to car-burying",1,1,0)
     
 --[[
 -- debug collision points
 for p in all(cf.coll_pts) do
  local px,py=p[1],p[2]
  pset(cf.carx+px,cf.cary+py,-7)
 end
 -- debug collision masks
 for row=1,16 do
  local mask=cf.collmasks[row]
  local y=(row-1)*8
  for bit=0,15 do
   if mask&(1<<bit)~=0 then
    local x=8*bit
    rect(x,y,x+7,y+7,-7)
   end
  end
 end
 --]]
end
-->8
-- car-burying
cb={}

function cb_enter(args)
 cb={
  bx=args.bx,
  by=args.by,
  w=args.w,
  h=args.h,
  grid=args.grid,
  dirtx=args.dirtx,
  dirty=args.dirty,
  dirth=args.dirth,
  carx=args.carx,
  cary=args.cary,
  wheel_r=args.wheel_r,
  cx=1,
  interactive=false,
  diggerx=-8,
  walk_a=anim(sid_walking,8),
  dig_a=anim(sid_digging,4),
  window_r={30,1,38,1},
  armx=34,
  army=7,
 }
 -- determine which grid cells
 -- are occupied by the car.
 hull_pts={
  --{0,8},{8,8},{16,5},{21,0},{29,0},{37,0},{45,0},{52,0},{57,8},
  -- move points slightly inward
  -- to make sure a single pixel
  -- in a cell doesn't mark it as full
  {3,11},{10,10},{18,5},{22,2},{30,2},{38,2},{46,2},{52,2},{55,8},{55,16},{57,20},
 }
 local cbx,cby=cb.carx-cb.bx,cb.cary-cb.by
 for p in all(hull_pts) do
  local px,py=cbx+p[1],cby+p[2]
  local gx,gy=1+px\8,1+py\8
  if gx>0 and gx<=cb.w and
     gy>0 and gy<=cb.h then
   for y=gy,cb.h do
    if cb.grid[y][gx]==sid_empty then
     cb.grid[y][gx]=sid_invisible
    end
   end
  else
   -- todo: this would indicate
   -- the car is not fully buried,
   -- so game over?
  end
 end
end            

function cb_update()
 bg_update()
 if cb.interactive then
  local dx=0
  if (btnp(⬅️)) dx-=1
  if (btnp(➡️)) dx+=1
  cb.cx=clamp(cb.cx+dx,1,cb.w)
 else
  -- digger walks in, window rolls down
  cb.window_r[4]=min(7,cb.window_r[4]+0.125)
  cb.diggerx+=0.5
  if cb.diggerx>=cb.bx-8 then
   cb.interactive=true
  end
 end
end

function cb_draw()
 -- draw bg
 bg_draw()
 -- draw board
 map(2,9,cb.bx-8,cb.by,12,7)
 local by=cb.by
 for y=1,cb.h do
  local bx=cb.bx
  for x=1,cb.w do
   spr(cb.grid[y][x],bx,by)
   bx+=8
  end
  by+=8
 end
 -- draw car
 rspr(cb.carx+11,
      cb.cary+21,cb.wheel_r,
      m_wheelx,m_wheely,m_wheelw,
      true,0.75)
 rspr(cb.carx+47,
      cb.cary+21,cb.wheel_r+0.17,
      m_wheelx,m_wheely,m_wheelw,
      true,0.75)
 spr(sid_car,cb.carx,
     cb.cary,8,3)
 -- draw car window and dolan
 clip(cb.carx+cb.window_r[1],
      cb.cary+cb.window_r[2],
      1+cb.window_r[3]-cb.window_r[1],
      1+cb.window_r[4]-cb.window_r[2])
 rectfill(cb.carx+cb.window_r[1],
          cb.cary+cb.window_r[2],
          cb.carx+cb.window_r[3],
          cb.cary+cb.window_r[4],
          -16)
 spr(sid_dolan_head,
     cb.carx+cb.window_r[1],
     cb.cary+cb.window_r[2])
 clip()
 -- draw dolan arm
 if cb.interactive then
  poke(0x5f2d,1) -- mouse
  local ax,ay=cb.carx+cb.armx,
              cb.cary+cb.army
  local mx,my=stat(32),stat(33)
  local theta=atan2(mx-ax,ay-my)
  line(ax+7*cos(theta),
       ay-7*sin(theta),
       mx,my,8)
  rspr(ax,ay,theta,
       m_armx,m_army,m_armw)
 end
 -- draw dirt pile
 local dsx,dsy=8*(sid_dirt_pile%16),
               8*(sid_dirt_pile\16)
 sspr(dsx,dsy,16,16,cb.dirtx,cb.dirty,
      16,-cb.dirth,false,true)
 -- draw digger
 if cb.interactive then
  spr(b.digger_a:s(),
      cb.bx-16+8*cb.cx,
      cb.by-16,2,2)
 else
  -- draw walking
  spr(cb.walk_a:s(),
      cb.diggerx,cb.by-16,1,2)
 end
 -- debug
 -- draw cells filled in by car
 --[[
 for y=1,cb.h do
  local row=cb.grid[y]
  local ry=cb.by+8*y-8
  for x,s in pairs(row) do
   local rx=cb.bx+8*x-8
   if s==sid_invisible then
    rect(rx,ry,rx+7,ry+7,-2)
   end
  end
 end
 --]]
end
__gfx__
00000000955299999999999999999999999999999999999944442444999999999999999999939999999339999999999999999999955299999559999900000000
000000009555599999999999955299999999999999bbbb9944444444999966666665999999339999993333999999999999999999955559995659999900000000
007007009fff99999999999995555999999999999bb7bbb9454444449999665555555999933399999333333995529999999999999fff99996599999900000000
000770009fff9999999999999fff9999999999999b7bbbb94444444e9996665555665599333333333333333395555999999999999fff99949999999900000000
0007700011f11999999999999fff9999999999999bbbbbb944424444996655556655559933333333999339999fff99999999999911f119499949999900000000
00700700111119999999999911f11999999999999bbbbbb954444444966555566555559993339999999339999fff199999999999111114f99999999900000000
00000000f111f99999999999111119999999999999bbbb99444444549655556555555559993399999993399991f1199999999999f1114f994999999900000000
00000000f111f999559999994111f9999999999999999999414444449655555555551559999399999993399911111999999999999f1499999999999900000000
999999994f44444466599999f411f999999999999999999955544d44965555555555155999666599aa144d44f111f99999999999954599999999999900000000
949999999ddd9f99559999999f45f999999999999988889955511424955655555551155996555559aa1114244111f9999999999994dd99999999999900000000
464999449d9d9999999999999dd4f99999999999988f888955554444955655555115551965555519111144449f45f999999999994d9d99999999999900000000
444494469d9d9999999999999d9d49999999499998f888895554d4449565555555555519556555551114d4449dd44999949999999d9d99999999999900000000
444d44449d9d9999999999999d9d9499999999999888888955411444956555115555551995555155554114449d9d9499444999999d9d99999999999900000000
644444649d9d9999999999999d9d99499949999998888889551544d4995551555551119991551551551544d49d9d9944665999999d9d99999999999900000000
444444449d9d9999999999999d9d999459999999998888995555d4449991155555519999991551195555d4449d9d9999559999999d9d99999999999900000000
44644444559559999999999955955995659949999999999955541444999911511119999999911999555414445595599994999999559559999999999900000000
4444d4449552999999999999999999999999999999999999555555559999999900000000aaa1111a999999999999999999999999955299999999999900000000
444444449555599999999999999999999999999999222299555555559999999900000000aaa1111a999999999552999999999999955559999999999900000000
464444449fff9999999999999552999999999999922e2229555555559994494900000000111111119999999995555999999999999fff99999559999900000000
444444469fff999999999999955559999999999992e2222955555555494444440000000011111111999999999fff9999999999999fff99995659999900000000
444d444411f11999999999999fff9999999999999222222955555555999444490000000055555555999999999fff19999999999911f119996599999900000000
6444446411111999999999999fff99999999999992222229555555559999999900000000555555559999999991f1199999999999111119949999999900000000
44444444f111f9999999999911f11999999999999922229955555555999999990000000055555555999999991111199999999999f11119499999999900000000
446444444411f999999999991111199999999999999999995555555599999999000000005555555599999999f111f99994999999f111f4999999999900000000
4444d4449f44f999999999994111f99999999999999999994245555599999999000000004241aa1199999999f111f999444999999f554f999999999900000000
444444449ddd449955999999f411f9999999999999aaaa99444d45559999999900000000444d4a119999999944444444665999999df499999999999900000000
464444449d9d9944665999999f45f999999999999aa7aaa94d44155599999999000000004d441111999999999ddd9999559999999d4d99999999999900000000
444444469d9d9999559999999dd4f999999999999a7aaaa944415555999949490000000044411111999999999d9d999994999999949d99999999999900000000
444d44449d9d9999999999999d9d4999999999499aaaaaa94d54455594994994000000004d544555999999999d9d9999999999994d9d99999999999900000000
649444949d9d9999999999999d9d9449999949999aaaaaa944441555994999490000000044441555999999999d9d9999999499999d9d99999999999900000000
499944949d9d9999999999999d9d99944599999999aaaa9942445555999494490000000042445555999999999d9d9999999999999d9d99999999999900000000
99999999559559999999999955955999999999999999999944414555999449990000000044414555999999995595599999999999559559999999999900000000
99999999999999999999999995529999999999999999999999999999999999999999999995529999999999999552999999999999955299989999999900000000
99999999999999444999999995555999955299999999999999999999999999925599999995555999999999999555599999999999955559899999999900000000
9999999999999444449999999fff999995555999999999999999999999999955559999998ff89999999999999fff9999999999999fff89999999999900000000
9999999999944444464499999fff99999fff99999999999999999999999f999fff9999999fff9999999999999fff9999999999999ff899999999999900000000
99999999999444444444499911f199999fff99999999999999999999999f999fff99999911f119999999999911f119999999999911f889999999999900000000
9999999999444446444444991111999911f199999999999999999999999ff111f111ff9911111999999999991111199999999999111889999999999900000000
999999999944444444446499f11199991111999999999999999999999999999111999f99f111f99999999999f111f99999999999ff8899999999999900000000
99999999944d444444444449f1119999f111f99999999999999999999999999111999f99f111f999559999999f119f9999999999911899999999999900000000
999999999444444444446449ff6699999f1f99999999999999999999999999911199999944444444665999999555999999999999955599999999999999999999
9999999999444446444444999ddd999996f69999999999999999999995999996669999999ddd9999559999999ddd9999999999999dd899999999999999999999
9999999994644444444444499ddd99999ddd9999999999999999999995dddddddd9999999d9d999999999999449d9999999999999d9899999999999999999999
9999999994444444444444499d919999911d9999999999999999999999999999919999999d9d9999999999999d449999999999999d8889999999999999999999
99999999994444444d4444499d919999991d9999999999999999999999999999919999999d9d9999999999999d9d4499559999999d9899999999999999999999
9999999994444444444444999d91999995d19999999999999999999999999999911115999d9d9999999999999d9d9944665999999d8d88999999999999999999
99999999944444d4444444499d91999995d99999999999999999999999999999999995999d9d9999999999999d9d9999559999999d9d98995599999995529989
99999999944444444444d49995555999995599999999999999999999999999999999999955955999999999995595599999999999444444446659999985555988
9999999999999999999999999999999999999999bbbbbbbbbbbbbbbb999999925599999999999999999999999552999999999999995529989999999900000000
9999999999999999999999999999999999999999bbbbbbbbbbbbbbbb999999555599999999955299999999999555599999999999995555899989989900000000
9999999999999999993999999999999999999999bbbbbaaaaaabbbbb9999999fff99999999955559999999999fff999999999999999989998999999900000000
9999999999999999993999999999999999999999bbbbaaaaaaaabbbb9999999fff999999999ff899999999999fff999999999999999899999999999900000000
9999999999999999993999999999999999999999bbbaaaaaaaaaabbb99999991f1199999899fff999999999911f1199999999999999889999999999900000000
9999999999999999393999999999999999999999bbaaaaaaaaaaaabb99999991111999999911f119999999991111119999999999988899999999999900000000
9999999999999999333939999999993999999999bbaaaaaaaaaaaabb9999999111f999999111111999999999fff1fff999999999f88999999999999900000000
9999999999999999993339999999393999999999bbaaaaaaaaaaaabb999999f1fff9999991f111f9999999999111999999999999889999999999999900000000
9999999999999999993999999999393939999999bbaaaaaaaaaaaabb999999966699999995f119f9999999999555999999999999889999999999999900000000
9999999999999999993999999999933939999999bbaaaaaaaaaaaabb9999999ddd9999999dfd99f9999999999ddd999999999999889999999999999900000000
999999999999ffff993999999999993399999999bbaaaaaaaaaaaabb9999999d91999999449d99f9999999999d9d999999999999889999999999999900000000
99999999f99bffffffff999ff9bf99399999bfffbbbaaaaaaaaaabbb9999999d919999999d449999999999999d9d999999999999889999999999999900000000
99999999ffbffffffffff9bffffff939bffbffffbbbbaaaaaaaabbbb9999999d919999999d9d4499559999999d9d999999999999988999999999999900000000
99999999fffffffffffffbffffffffffffffffffbbbbbaaaaaabbbbb9999999d919999999d9d99446659999944449999559999999d8898999999999900000000
99999999ffffffffffffffffffffffffffffffffbbbbbbbbbbbbbbbb9999999d919999999d9d9999559999999d9d4444665999999d8d98999999999900000000
9999999911111111111111111111111111111111bbbbbbbbbbbbbbbb999999555599999955955999999999995595599955999999444444449999999900000000
99977999999999999999999999999666666666666666666666666666666669999999999999944999000000009999900000099999999999999999666600000000
99779999999999999999999999996111111166111111111666111111111116999999999999944449000000009990000000000999999666699996666600000000
9997799999999999999999999996111c11116611c111111616611111c111116999999999999944490000000099000010100100999966ddd66666d66600000000
9977799999999999999999999961c1c11111661c11111116166111111c111169999999999999ff49000000009000000000000009966d666ddddd666600000000
997799999999999999999999961c111111116611111111161166111111111116999999999999fff900000000900100555500000996d666666666666600000000
9997999999999999999999996111111111116611111111161166111111111c16999999999999ffe90000000000100566665000009566d6666666666600000000
9999999999999999999999961111111111116611111111161116611111111111699999999999ee2900000000000056666665001095566d666666666600000000
99999999999999999999996111111111111166111111111611166611111111116999999999992229000000000000566666650000955666d66666666600000000
99779999966666666666666666666666666666666666666666666666666666666699999999999999999999990000566666650000995666666699666600000000
977799996666666666666666666666666dd6666666666dd6666666666666666666999999999999999999999901005666666501009556666d6999ddd600000000
997799996dddddddddddd66666666666666666666666666666666666666666666899999999999999999999990000056666500000555666666999999d00000000
9997779977777666666666666666666666666666666666666666666666666666689999999999999999999999900000555500000955dddd669999999900000000
9997779977666666666666666666666666666666666666666666666666666666689999999999999999999999900100010000000995dddddd9999999900000000
99979999766666666666666666666666666666666666666666666666666666666899999999999999999999999900000000010099999d99dd9999999900000000
999999996666666666666ddd66666666666666666666666666666666666666666899999999999999999999999990000010000999999999999999999900000000
999999996666666666666666d66666666666666666666666666666666666666666999999222ef999999999999999900000099999999999999999999900000000
0000000066666666666666666d66666666666666666666666666dddddd6666666699999999990009999999999999999999999999999999999999999900000000
0000000066666666ddddddd6666666666666666666666666666d999999d666666699999999999999999999999999999999999999999999999999999900000000
000000006666666d9999999d66666666666666666666666666d99999999d66666699999999999999999955999999999999999999999999999999999900000000
000000009d6666d999999999d666666666666666666666666d9999999999ddddddd9999999999999995555599949999999999999999999999999999900000000
0000000099d6dd99999999999dddddddddddddddddddddddd5999999999995ddddd9999999999999555555999499999999999449999999999999999900000000
000000009995dd999999999999555555555555555555555555999999999995dddd59999999999999955559999949999999994999999999999999999900000000
00000000999955999999999999999999999999999999999999999999999995555599999999999999999999999999999999449999999999999999999900000000
00000000999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999900000000
00000000000000000000000000000000000000000000000000000000000000000000000099999999999999999999999999999999999999999999999900000000
00000000000000000000000000000000000000000000000000000000000000000000000099999999999999999999949999999999999999999999999900000000
00000000000000000000000000000000000000000000000000000000000000000000000099999999999999999999994999999999999999999999999900000000
00000000000000000000000000000000000000000000000000000000000000000000000099999999999999999999994999949999999999999999999900000000
0000000000000000000000000000000000000000000000000000000000000000000000009999999955555599999999999944499999999999fff1199900000000
000000000000000000000000000000000000000000000000000000000000000000000000999999999555555599999999999449999999dd6ff1111f8500000000
00000000000000000000000000000000000000000000000000000000000000000000000099999999999955599999999999499999dddddd6f11111f8500000000
00000000000000000000000000000000000000000000000000000000000000000000000099999999999999999999999999999999dddddd61111118f500000000
__label__
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc777777777777777cccccccccccccccccccccccccccccc6677777777666ccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77777777777777777cccccccccccccccccccccccccccc667777777777666cccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77777777777777777cccccccccccccccccccccccccccc677777777777766cccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc7777777777777777cc777777777777cccccccccccccccc6777777777766ccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc7777777777777ccc77777777777777ccccccccccccccccc77777777cccccccccccc
ccccccccccccccccccccccccccccc6666666cccccccccccccccccccccccccc77777777ccccccc7777777777777cccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccc7777777777777777cccccccccccccccccccccccccccccccc777777777777777777777777cccccccccccccccccccccccccccc
ccccccccccccccccccccccccccc777777777999977777c9999cc99cccccc999999cc9999cccc7799777777999977777777777ccccccccccccccccccccccccccc
cccccccccccccccccccccccccccc77777777999917777c99991c991ccccc9999991c99991cccc799177777999917777777777777cccccccccccccccccccccccc
ccccccccccccccccccccccccccccc77777779911997799c1991c991ccccc9911991c991199cc99c11777997111177777777777777ccccccccccccccccccccccc
cccccccccccccccccccccccccccccc77777799179917991c991c991ccccc991c991c991c991c991ccccc991ccc67777777777777cccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccc991c991c991c991c991ccccc9999991c991c991cc11ccccc999999cc66666666cccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccc991c991c991c991c991ccccc9999991c991c991ccccccccc9999991ccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccc991c991c991c991c991ccccc9911991c991c991cccccccccc111991ccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccc991c991c991c991c991ccccc991c991c991c991ccccccccccccc991ccccccccccccccccccccccccccccccccccccc
cccccccccccccc777777777ccccccccccccc9999991c9999c11c999999cc991c991c991c991ccccccccc9999c11ccccccccccccccccccccccccccccccccccccc
cccccccccccc7777777777777ccccccccccc9999991c99991ccc9999991c991c991c991c991ccccccccc99991ccccccccccccccccccccccccccccccccccccccc
ccccccccccc777777777777777ccccccccccc111111cc1111cccc111111cc11cc11cc11cc11cccccccccc1111ccccccccccccccccccccccccccccccccccccccc
666666666cc777777777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
7777777776667777777777777ccccccccc9999cc999999cc9999cccc999999cc99cccccc99cccccc999999cccc9999cccccccccccccccccccccccccccccccccc
7777777777666677777777766ccccccccc99991c9999991c99991ccc9999991c991ccccc991ccccc9999991ccc99991ccccccccccccccccccccccccccccccccc
777777777766666666666666cccccccc99c1111c9911991c991199ccc199111c991ccccc991ccccc9911991c99c1111ccccccccccccccccccccccccccccc7777
77777777766ccc66666666cccccccccc991ccccc991c991c991c991ccc991ccc991ccccc991ccccc991c991c991ccccccccccccccccccccccccccccccc777777
666666666ccccccccccccccccccccccc991ccccc9999991c991c991ccc991ccc991ccccc991ccccc9999991c991cccccccccccccccccccccccccccccc7777777
cccccccccccccccccccccccccccccccc991ccccc9999991c991c991ccc991ccc991ccccc991ccccc9999991c991ccccccccccccccccccccccccccccc77777777
cccccccccccccccccccccccccccccccc991ccccc9911991c991c991ccc991ccc991ccccc991ccccc9911991c991ccccccccccccccccccccccccccccc77777777
cccccccccccccccccccccccccccccccc991ccccc991c991c991c991ccc991ccc991ccccc991ccccc991c991c991cccccccccccccccccccccccccccccc7777777
ccccccccccccccccccccccccccccccccc19999cc991c991c9999991c999999cc999999cc999999cc991c991cc19999cccccccccccccccccccccccccccccc7777
cccccccccccccccccccccccccccccccccc99991c991c991c9999991c9999991c9999991c9999991c991c991ccc99991ccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccc1111cc11cc11cc111111cc111111cc111111cc111111cc11cc11cccc1111ccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccc666666666666ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccc66666666666666cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccc666777777776ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccc777777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccc77777777777777ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccc77777777777777ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccc777777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccc77777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc66666666666666666666666666666666ccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccc6111111166111111111666111111111116cccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccc6111c11116611c111111616611111c111116ccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccc61c1c11111661c11111116166111111c11116ccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccc61c111111116611111111161166111111111116cccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccc6111111111116611111111161166111111111c16cccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccc611111111111166111111111611166111111111116ccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccc6111111111111166111111111611166611111111116ccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccc666666666666666666666666666666666666666666666666666666666cccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccc6666666666666666666666666dd6666666666dd6666666666666666666cccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccc6dddddddddddd666666666666666666666666666666666666666666668cccccccccccccccccccccccccccccccccccc
cccccccccc3ccccccccccccccc3ccccccc7777766666666666666666666666666666666666666666666666666668cccccccccccccc3ccccccccccccccccccccc
cccccccccc3ccccccccccccccc3ccccccc7766666666666666666666666666666666666666666666666666666668cccccccccccccc3ccccccccccccccccccccc
cccccccccc3ccccccccccccccc3ccccccc7666666666666666666666666666666666666666666666666666666668cccccccccccccc3ccccccccccccccccccccc
cccccccc3c3ccccccccccccc3c3ccccccc6666666666666ddd666666666666666666666666666666666666666668cccccccccccc3c3ccccccccccccccccccccc
cccccccc333c3ccccccccccc333c3ccccc6666666666666666d666666666666666666666666666666666666666663ccccccccccc333c3ccccccccc3ccccccccc
cccccccccc333ccccccccccccc333ccccc66666666666666666d66666666666666666666666666dddddd666666663ccccccccccccc333ccccccc3c3ccccccccc
cccccccccc3ccccccccccccccc3ccccccc66666666ddddddd6666666666666666666666666666d010000d6666666cccccccccccccc3ccccccccc3c3c3ccccccc
cccccccccc3ccccccccccccccc3ccccccc6666666d0050000d66666666666666666666666666d00005000d666666cccccccccccccc3cccccccccc33c3ccccccc
ccccffffcc3cccccccccffffcc3ccccccccd6666d005666500d666666666666666666666666d0000666000dddddddcccccccffffcc3ccccccccccc33cccccccc
fccbffffffffcccffccbffffffffcccffcbfd6dd00066666500dddddddddddddddddddddddd5000666660105dddddccffccbffffffffcccffcbfcc3cccccbfff
ffbffffffffffcbfffbffffffffffcbffffff5dd00066666000f555555555555555555555555005666665105dddd5cbfffbffffffffffcbffffffc3cbffbffff
fffffffffffffbfffffffffffffffbffffffff5500566666000ffffffffffbffffffffffffff1016666600055555fbfffffffffffffffbffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffff00056665000fffffffffffffffffffffffff00006660000fffffffffffffffffffffffffffffffffffffffff
11111111111111111111111111111111111111110000005010011111111111111111111111110000050010011111111111111111111111111111111111111111
aaaa1111aaaa1111aaaa1111aaaa1111aaaa1111a001100100aa1111aaaa1111aaaa1111aaaa101000000011aaaa1111aaaa1111aaaa1111aaaa1111aaaa1111
aaaa1111aaaa1111aaaa1111aaaa1111aaaa1111aa0000000aaa1111aaaa1111aaaa1111aaaa110001000111aaaa1111aaaa1111aaaa1111aaaa1111aaaa1111
11111111111111111111111111111111111111111111110111111111111111111111111111111111001111111111111111111111111111111111111111111111
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555777577757775577557755555577777555555777557755555577577757775777577755555555555555555555555555555555
55555555555555555555555555555707070707000750075005555770007755555570075705555750057007070707057005555555555555555555555555555555
55555555555555555555555555555777077507755777577755555770757705555570570705555777557057770775057055555555555555555555555555555555
55555555555555555555555555555700070757005507050705555770507705555570570705555507057057070707557055555555555555555555555555555555
55555555555555555555555555555705570707775775077505555577777005555570577505555775057057070707057055555555555555555555555555555555
55555555555555555555555555555505550505000500550055555550000055555550550055555500555055050505055055555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555

__gff__
0000000000000001010000000000000000000000000000010101000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0000000000000000000000000000000045454545454545444545454545454545000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000045414245454545454544454545454345000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000045454545454545454545454545454545000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000045444545454541424545454345454545000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000045454545454545454545454545454544000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000045454545454545454545414245454545000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000045454345454545454545454545454545000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000061626364616263646162616263646162000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
7172737471727374717271727374717271727374717273747172717273747172000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
29291a0606060606060606060639292929292929292929292929292929292929000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2626160606060606060606060636262626262626262626262626262626262626000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2626160606060606060606060636262626262626262626262626262626262626000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2626160606060606060606060636262626262626262626262626262626262626000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2626160606060606060606060636262626262626262626262626262626262626000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2626160606060606060606060636262626262626262626262626262626262626000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2626160606060606060606060636262626262626262626262626262626262626000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
008b8c0000990000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
009b9c0000a90000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
01010400183501f350283503035000000000000000000000000000000000000000000100029000280002600021000200001e00002000030000400004000000000000000000000000000000000000000000000000
000200001835018350183501f3501f3501c3401c3101c310000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000200001835018350183501335013350103401031010310000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
19060c00184501d4401f44024430184201d4201f41024410184001d4001f400244000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011010000c550005000c5500050013550005001155013550145500050011550005001055000500135501455000000000000000000000000000000000000000000000000000000000000000000000000000000000
010800001c753000030000000000000000000000000000003c63500000000000000000000000000000000000000000000000000000001c7530000000000000003c63500000000000000000000000000000000000
001000000c120001200c1200012011120051201112005120101200412010120101200412010120141201412000000000000000000000000000000000000000000000000000000000000000000000000000000000
48020020056100e610046100361011610036100561005610066101061006610076100761008610086100861013610076100761008610086100861013610076100961009610096100961013610076100661006610
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
05181800181501814018140181450c100181401815018140181401815018140131401815018140181401815018140131401816018160181601816518100181000010000100001000010000100001000010000100
011818001f4501f4401f4401f445134001f4401f4501d4401f440214501f4401d4401f4501d4401f440214501f4401d4402446024460244602446524400244000040000400004000040000400004000040000400
011818001c2501c2401c2401c245002001c2401c2501c2401c2401d2501c2401a2401c2501c2401c2401d2501c2401a2401c2601c2601c2601c2651c2001c2000020000200002000020000200002000020000200
__music__
03 41420407
03 41050407
00 41787a79

