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
-- rotating sprite map locations
m_wheelx,m_wheely,m_wheelw=2,18,2
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
  digger_t=0,
  dirtx=0,
  dirty=72,
  dirth=0,
  clampx=function(this,x)
   return max(1,min(this.w,x))
  end,
  clampy=function(this,y)
   return max(1,min(this.h,y))
  end,
  cursor_fill=function(this)
   if this.selecting then
    this.crs_t=1+this.crs_t%4
    return this.crs_fills[this.crs_t]
   else
    this.crs_t=0
    return this.crs_fills[1]
   end
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
 b.digger_t+=1
 spr(sid_digging[1+((b.digger_t\4)%8)],
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

sid_walk1={67,69}
sid_walk2_top=64
sid_walk2_bot={80,96,80,112}
walk1=anim(sid_walk1,8)
walk2=anim(sid_walk2_bot,{4,8,4,8})
walk1x=30
walk2x=120
walk1dx=0.5
walk2dx=0.5
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
 -- draw walk cycle 1
 walk1x=walk1x<00 and 30 or walk1x-walk1dx
 spr(walk1:s(),walk1x,40,2,2)
 -- draw walk cycle 2
 walk2x=walk2x<90 and 120 or walk2x-walk2dx
 spr(sid_walk2_top,walk2x,40,1,1)
 spr(walk2:s(),walk2x,48,1,1)
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
   carx2+=pxs-px
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
   -- flip velocity
   -- todo: clamp small vx to zero
   --       and advance.
   cf.carvx*=-0.5
   if abs(cf.carvx)<0.25 then
    cf.carvx=0
   end
   break
  end
 end
 cf.carx=carx2
 -- y
 -- terminal velocity of 8 to avoid
 -- tunneling through an entire tile
 cf.carvy=min(8,cf.carvy+cf.gravity)
 local cary2=cf.cary+cf.carvy
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
   cary2+=pys-py
   -- zero velocity
   cf.carvy=0
   break
  end
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
 }
 -- determine which grid cells
 -- are occupied by the car.
 -- this code hard-codes some
 -- assumptions about the shape
 -- of the car sprite.
 local cgx,cgy=1+(cb.carx-cb.bx)\8,
               1+(cb.cary-cb.by)\8
 local heights={1,1,0,0,0,0,0,0}
 for i=1,#heights do
  local x=cgx+i-1
  if (x>cb.w) break
  for y=cgy+heights[i],cb.h do
   if y>0 and cb.grid[y][x]==sid_empty then
    cb.grid[y][x]=sid_invisible
   end
  end
 end
end            

function cb_update()
 bg_update()
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
 -- draw dirt pile
 local dsx,dsy=8*(sid_dirt_pile%16),
               8*(sid_dirt_pile\16)
 sspr(dsx,dsy,16,16,cb.dirtx,cb.dirty,
      16,-cb.dirth,false,true)
 -- debug
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
999999999ddd9f99559999999f45f999999999999988889955511424955655555551155996555559aa1114244111f9999999999994dd99999999999900000000
999999999d9d9999999999999dd4f99999999999988f888955554444955655555115551965555519111144449f45f999999999994d9d99999999999900000000
999999999d9d9999999999999d9d49999999499998f888895554d4449565555555555519556555551114d4449dd44999949999999d9d99999999999900000000
999999999d9d9999999999999d9d9499999999999888888955411444956555115555551995555155554114449d9d9499444999999d9d99999999999900000000
999999999d9d9999999999999d9d99499949999998888889551544d4995551555551119991551551551544d49d9d9944665999999d9d99999999999900000000
999999999d9d9999999999999d9d999459999999998888995555d4449991155555519999991551195555d4449d9d9999559999999d9d99999999999900000000
99999999559559999999999955955995659949999999999955541444999911511119999999911999555414445595599994999999559559999999999900000000
999999999552999999999999999999999999999999999999555555559999999900000000aaa1111a999999999999999999999999955299999999999900000000
999999999555599999999999999999999999999999222299555555559999999900000000aaa1111a999999999552999999999999955559999999999900000000
999999999fff9999999999999552999999999999922e2229555555559994494900000000111111119999999995555999999999999fff99999559999900000000
999999999fff999999999999955559999999999992e2222955555555494444440000000011111111999999999fff9999999999999fff99995659999900000000
9999999911f11999999999999fff9999999999999222222955555555999444490000000055555555999999999fff19999999999911f119996599999900000000
9999999911111999999999999fff99999999999992222229555555559999999900000000555555559999999991f1199999999999111119949999999900000000
99999999f111f9999999999911f11999999999999922229955555555999999990000000055555555999999991111199999999999f11119499999999900000000
999999994411f999999999991111199999999999999999995555555599999999000000005555555599999999f111f99994999999f111f4999999999900000000
000000009f44f999999999994111f99999999999999999994245555599999999000000004241aa1199999999f111f999444999999f554f999999999900000000
000000009ddd449955999999f411f9999999999999aaaa99444d45559999999900000000444d4a119999999944444444665999999df499999999999900000000
000000009d9d9944665999999f45f999999999999aa7aaa94d44155599999999000000004d441111999999999ddd9999559999999d4d99999999999900000000
000000009d9d9999559999999dd4f999999999999a7aaaa944415555999949490000000044411111999999999d9d999994999999949d99999999999900000000
000000009d9d9999999999999d9d4999999999499aaaaaa94d54455594994994000000004d544555999999999d9d9999999999994d9d99999999999900000000
000000009d9d9999999999999d9d9449999949999aaaaaa944441555994999490000000044441555999999999d9d9999999499999d9d99999999999900000000
000000009d9d9999999999999d9d99944599999999aaaa9942445555999494490000000042445555999999999d9d9999999999999d9d99999999999900000000
00000000559559999999999955955999999999999999999944414555999449990000000044414555999999995595599999999999559559999999999900000000
99255999999999999999999999999992559999999999999255999999999999999999999995529999999999999552999999999999955299989999999900000000
95555999999999444999999999999955559999999999995555999999999999925599999995555999999999999555599999999999955559899999999900000000
99fff99999999444449999999999999fff9999999999999fff99999999999955559999998ff89999999999999fff9999999999999fff89999999999900000000
99fff99999944444464499999999999fff9999999999999fff999999999f999fff9999999fff9999999999999fff9999999999999ff899999999999900000000
991f1199999444444444499999999991f119999999999991f1199999999f999fff99999911f119999999999911f119999999999911f889999999999900000000
99111199994444464444449999999991111999999999991111199999999ff111f111ff9911111999999999991111199999999999111889999999999900000000
99111f9999444444444464999999999111f999999999f9f111f999999999999111999f99f111f99999999999f111f99999999999ff8899999999999900000000
99111f99944d4444444444499999999111f9999999999f91119f99999999999111999f99f111f999559999999f119f9999999999911899999999999900000000
9966ff999444444444446449999999966ff99999999999966699f999999999911199999944444444665999999555999999999999955599999999999999999999
99ddd99999444446444444999999999ddd9999999999999ddd9f999995999996669999999ddd9999559999999ddd9999999999999dd899999999999999999999
99ddd99994644444444444499999999ddd999999999999d99d99999995dddddddd9999999d9d999999999999449d9999999999999d9899999999999999999999
99d9d99994444444444444499999999d9d99999999999d999d999999999999999d9999999d9d9999999999999d449999999999999d8889999999999999999999
99d9d999994444444d4444499999999d9d99999999999d999d999999999999999d9999999d9d9999999999999d9d4499559999999d9899999999999999999999
99d9d99994444444444444999999999d9dd59999999955999d999999999999999dddd5999d9d9999999999999d9d9944665999999d8d88999999999999999999
99d9d999944444d4444444499999999d99959999999999999d99999999999999999995999d9d9999999999999d9d9999559999999d9d98995599999995529989
95555999944444444444d49999999955999999999999999955999999999999999999999955955999999999995595599999999999444444446659999985555988
9f666f9999999999999999999999999999999999bbbbbbbbbbbbbbbb999999925599999999999999999999999552999999999999995529989999999900000000
99dddf9999999999999999999999999999999999bbbbbbbbbbbbbbbb999999555599999999955299999999999555599999999999995555899989989900000000
99ddd99999999999993999999999999999999999bbbbbaaaaaabbbbb9999999fff99999999955559999999999fff999999999999999989998999999900000000
99d9d99999999999993999999999999999999999bbbbaaaaaaaabbbb9999999fff999999999ff899999999999fff999999999999999899999999999900000000
99d9d99999999999993999999999999999999999bbbaaaaaaaaaabbb99999991f1199999899fff999999999911f1199999999999999889999999999900000000
9d99d99999999999393999999999999999999999bbaaaaaaaaaaaabb99999991111999999911f119999999991111119999999999988899999999999900000000
9d999d9999999999333939999999993999999999bbaaaaaaaaaaaabb9999999111f999999111111999999999fff1fff999999999f88999999999999900000000
5599559999999999993339999999393999999999bbaaaaaaaaaaaabb999999f1fff9999991f111f9999999999111999999999999889999999999999900000000
996ff99999999999993999999999393939999999bbaaaaaaaaaaaabb999999966699999995f119f9999999999555999999999999889999999999999900000000
99ddd99999999999993999999999933939999999bbaaaaaaaaaaaabb9999999ddd9999999dfd99f9999999999ddd999999999999889999999999999900000000
99ddd9999999ffff993999999999993399999999bbaaaaaaaaaaaabb9999999d9d999999449d99f9999999999d9d999999999999889999999999999900000000
999dd999f99bffffffff999ff9bf99399999bfffbbbaaaaaaaaaabbb9999999d9d9999999d449999999999999d9d999999999999889999999999999900000000
999ddd99ffbffffffffff9bffffff939bffbffffbbbbaaaaaaaabbbb9999999d9d9999999d9d4499559999999d9d999999999999988999999999999900000000
999d9d99fffffffffffffbffffffffffffffffffbbbbbaaaaaabbbbb9999999d9d9999999d9d99446659999944449999559999999d8898999999999900000000
999d9d59ffffffffffffffffffffffffffffffffbbbbbbbbbbbbbbbb9999999d9d9999999d9d9999559999999d9d4444665999999d8d98999999999900000000
9955959911111111111111111111111111111111bbbbbbbbbbbbbbbb999999555599999955955999999999995595599955999999444444449999999900000000
99977999999999999999999999999666666666666666666666666666666669999999999999999999000000009999900000099999999999999999666600000000
99779999999999999999999999996111111166111111111666111111111116999999999999990999000000009990000000000999999666699996666600000000
9997799999999999999999999996111c11116611c111111616611111c111116999999999999900090000000099000010100100999966ddd66666d66600000000
9977799999999999999999999961c1c11111661c11111116166111111c1111699999999999990009000000009000000000000009966d666ddddd666600000000
997799999999999999999999961c111111116611111111161166111111111116999999996666ff0600000000900100555500000996d666666666666600000000
9997999999999999999999996111111111116611111111161166111111111c16999999996dddfff60000000000100566665000009566d6666666666600000000
9999999999999999999999961111111111116611111111161116611111111111699999996ddd222600000000000056666665001095566d666666666600000000
9999999999999999999999611111111111116611111111161116661111111111699999996ddd2226000000000000566666650000955666d66666666600000000
997799999666666666666666666666666666666666666666666666666666666666999999bbbbbbbbbbbbbbbb0000566666650000995666666699666600000000
977799996666666666666666666666666dd6666666666dd6666666666666666666999999bbbbbbbbbbbb9bbb01005666666501009956666d6999ddd600000000
997799996dddddddddddd666666666666666666666666666666666666666666668999999bbbbbbbbbabbbbbb0000056666500000995666666999999d00000000
999777997777766666666666666666666666666666666666666666666666666668999999b000bbbba9000bbb9000005555000009995ddd669999999900000000
999777997766666666666666666666666666666666666666666666666666666668999999bbbf2222babf222290010001000000099996dddd9999999900000000
999799997666666666666666666666666666666666666666666666666666666668999999bbbbbbbbbbbbbbbb9900000000010099999d99dd9999999900000000
999999996666666666666ddd666666666666666666666666666666666666666668999999bbbbbbbbbbbbbbbb9990000010000999999999999999999900000000
999999996666666666666666d66666666666666666666666666666666666666666999999bbbbbbbbbbbbbbbb9999900000099999999999999999999900000000
0000000066666666666666666d66666666666666666666666666dddddd6666666699999944444444999999999999999999999999999999999999999900000000
0000000066666666ddddddd6666666666666666666666666666d999999d666666699999944444444999999999999999999999999999999999999999900000000
000000006666666d9999999d66666666666666666666666666d99999999d666666999999444f0404999955999999999999999999999999999999999900000000
000000009d6666d999999999d666666666666666666666666d9999999999ddddddd99999ff420004995555599949999999999999999999999999999900000000
0000000099d6dd99999999999dddddddddddddddddddddddd5999999999995ddddd999996242fff4555555999499999999999449999999999999999900000000
000000009995dd999999999999555555555555555555555555999999999995dddd59999962d2fff6955559999949999999994999999999999999999900000000
00000000999955999999999999999999999999999999999999999999999995555599999942222226999999999999999999449999999999999999999900000000
00000000999999999999999999999999999999999999999999999999999999999999999961111116999999999999999999999999999999999999999900000000
00000000000000000000000000000000000000000000000000000000000000000000000044444444999999999999999999999999999999999999999900000000
00000000000000000000000000000000000000000000000000000000000000000000000044444444999999999999949999999999999999999999999900000000
000000000000000000000000000000000000000000000000000000000000000000000000ff440004999999999999994999999999999999999999999900000000
000000000000000000000000000000000000000000000000000000000000000000000000424f0004999999999999994999949999999999999999999900000000
00000000000000000000000000000000000000000000000000000000000000000000000062624f0455555599999999999944499999999999fff1199900000000
00000000000000000000000000000000000000000000000000000000000000000000000062d2fff69555555599999999999449999999dd6ff1111f8500000000
00000000000000000000000000000000000000000000000000000000000000000000000062222246999955599999999999499999dddddd6f11111f8500000000
00000000000000000000000000000000000000000000000000000000000000000000000061111116999999999999999999999999dddddd61111118f500000000
__label__
ccccccccccccccccccccccccccccccccccccccccccccccc7777777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccc777777777777777ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccc7777777777777777ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccc677777777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccc66666666666ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccc6666666ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccc677777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccc67777777777777ccccc9999cccccc9999cc99cccccc999999cc9999cccccc99cccccc9999cccccccccccccccccccccccccccccccccccccc
ccccccccccccccccc67777777777777ccccc99991ccccc99991c991ccccc9999991c99991ccccc991ccccc99991ccccccccccccccccccccccccccccccccccccc
ccccccccccccccccc777777777777777cccc991199cc99c1991c991ccccc9911991c991199cc99c11ccc99c1111ccccccccccccccccccccccccccccccccccccc
cccccccccccccccccc7777777777777ccccc991c991c991c991c991ccccc991c991c991c991c991ccccc991ccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccc777777777ccccccc991c991c991c991c991ccccc9999991c991c991cc11ccccc999999ccccccccccccccccccccccc77777777ccccccc
cccccccccccccccccccccccccccccccccccc991c991c991c991c991ccccc9999991c991c991ccccccccc9999991ccccccccccccccccccccc77777777776ccccc
cccccccccccccccccccccccccccccccccccc991c991c991c991c991ccccc9911991c991c991cccccccccc111991cccccccccccccccccccc777777777777777cc
c6677777777ccccccccccccccccccccccccc991c991c991c991c991ccccc991c991c991c991ccccccccccccc991ccccccccccccccccccc67777777777777777c
6777777777777ccccccccccccccccccccccc9999991c9999c11c999999cc991c991c991c991ccccccccc9999c11cccccccccccccccccccc677777777777777cc
77777777777777cccccccccccccccccccccc9999991c99991ccc9999991c991c991c991c991ccccccccc99991cccccccccccccccccccccccc6666666666ccccc
c777777777777cccccccccccccccccccccccc111111cc1111cccc111111cc11cc11cc11cc11cccccccccc1111ccccccccccccccccccccccccccccccccccccccc
cc7777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc77777777ccccccccccccccccccccccc9999cc999999cc9999cccc999999cc99cccccc99cccccc999999cccc9999cccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccc99991c9999991c99991ccc9999991c991ccccc991ccccc9999991ccc99991ccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccc99c1111c9911991c991199ccc199111c991ccccc991ccccc9911991c99c1111ccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccc991ccccc991c991c991c991ccc991ccc991ccccc991ccccc991c991c991ccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccc991ccccc9999991c991c991ccc991ccc991ccccc991ccccc9999991c991ccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccc991ccccc9999991c991c991ccc991ccc991ccccc991ccccc9999991c991ccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccc991ccccc9911991c991c991ccc991ccc991ccccc991ccccc9911991c991ccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccc991ccccc991c991c991c991ccc991ccc991ccccc991ccccc991c991c991ccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccc19999cc991c991c9999991c999999cc999999cc999999cc991c991cc19999cccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccc99991c991c991c9999991c9999991c9999991c9999991c991c991ccc99991ccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccc1111cc11cc11cc111111cc111111cc111111cc111111cc11cc11cccc1111ccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccc6666666666cccccccccccccccccccccccccccccccccccccc77777777ccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccc66666666666666cccccccccccccccccccccccccccccccccc7777777777777cccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccc7777777777766666cccccccccccccccccccccccccccccccc777777777777777ccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccc77777777777776666ccccccccccccccccccccccccccccccccc777777777777777cccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccc777777777776666ccccccccccccccccccccccccccccccccccc77777777777777cccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccc6666666666cccccccccccccccccccccccccccccccccccccc777777777777ccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77777777ccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc7777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccc666666666666666666666666666666666666666ccccccc7777777777ccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccc61111111111166611111161161111111111111166ccccc7777777777776ccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccc6111111111111666111111611661111111111111166ccccc77777777776666ccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccc61111111111111666111111611161111111111111166cccc6666666666666666cccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccc611111111111111666111111611166111111111111166cccc6666666666666666cccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccc6111111111111111666111111611166611111111111166ccccc66666666666666ccccccccccccccccc
cccccccccccccccccccccccccccccccccc66666666666d66666666666666666666666666666666666666666666668cccccc6666666666ccccccccccccccccccc
cccccccccccccccccccccccccccccccc6666666666666666666666666666666666666666666666666666666666668ccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccc66ddddddddddd66666666ddd6666666666666ddd666666666666666666688ccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccc67777766666666666666666666666d666666666666666d666666666666688ccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccc77766666666666666666666666666d666666666666666d666666666666688ccccccccccccccccccccccccccccccccccc
cccccccccccccccccc3ccccccccccccc77666666666666666666666666666d666666666666666d666666666666688ccccc3ccccccccccccccc3ccccccccccccc
cccccccccccccccccc3ccccccccccccc77666666666666666666666666666d666666666666666d666666666666688ccccc3ccccccccccccccc3ccccccccccccc
cccccccccccccccccc3ccccccccccccc77666666666666666666666666666d666666666666666d666666666666668ccccc3ccccccccccccccc3ccccccccccccc
cccccccccccccccc3c3ccccccccccccc66666666666666666666666666666d666666666666666d666666666666666ccc3c3ccccccccccccc3c3ccccccccccccc
cccccccccccccccc333c3ccccccccccc66666666666666666666666666666d666666666666666d666666666666666ccc333c3ccccccccccc333c3ccccccccc3c
cccccccccccccccccc333ccccccccccc66666666666666666666666666666d6666666666666666666666666666666ccccc333ccccccccccccc333ccccccc3c3c
3ccccccccccccccccc3ccccccccccccc66666666666666666666666666666d6666666666666666666666666666666ccccc3ccccccccccccccc3ccccccccc3c3c
3ccccccccccccccccc3ccccccccccccc66666666666666666666666666666d6666666666666666666666666666666ccccc3ccccccccccccccc3cccccccccc33c
ccccccccccccffffcc3cccccccccffffd66666666dddddd66666666666666666666666666dddddd66666666666666fffcc3cccccccccffffcc3ccccccccccc33
ccccbffffccbffffffffcccffccbffffdd6666dd6000002666666666666666666666666660000026666666666ddddfffffffcccffccbffffffffcccffcbfcc3c
bffbffffffbffffffffffcbfffbffffffdddddbf00000200dddddddddddddddddddddddd00000200dddddddddddddffffffffcbfffbffffffffffcbffffffc3c
fffffffffffffffffffffbfffffffffffffffbff00055000fffffffffffffffffffffbff00055000fffffffffffffffffffffbfffffffffffffffbffffffffff
fffffffffffffffffffffffffffffffffffffffff005500ffffffffffffffffffffffffff005500fffffffffffffffffffffffffffffffffffffffffffffffff
11111111111111111111111111111111111111111020000111111111111111111111111110200001111111111111111111111111111111111111111111111111
aa11aa11aa11aa11aa11aa11aa11aa11aa11aa11aa000011aa11aa11aa11aa11aa11aa11aa000011aa11aa11aa11aa11aa11aa11aa11aa11aa11aa11aa11aa11
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
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
4545454545454544454545454545454545454545454545444545454545454545000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
4541424545454545454445454545434545414245454545454544454545454345000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
4545454545454545454545454545454545454545454545454545454545454545000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
4544454545454142454545434545454545444545454541424545454345454545000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
4545454545454545454545454545454445454545454545454545454545454544000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
4545454545454545454541424545454545454545454545454545414245454545000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
4545434545454545454545454545454545454345454545454545454545454545000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
6162636461626364616261626364616261626364616263646162616263646162000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
7172737471727374717271727374717271727374717273747172717273747172000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
29291a0606060606060606060639292929292929292929292929292929292929000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2626160606060606060606060636262626262626262626262626262626262626000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2626160606060606060606060636262626262626262626262626262626262626000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2626160606060606060606060636262626262626262626262626262626262626000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2626160606060606060606060636262626262626262626262626262626262626000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2626160606060606060606060636262626262626262626262626262626262626000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2626160606060606060606060636262626262626262626262626262626262626000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
008b8c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
009b9c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
01010400183501f350283503035000000000000000000000000000000000000000000100029000280002600021000200001e00002000030000400004000000000000000000000000000000000000000000000000
000200001835018350183501f3501f3501c3401c3101c310000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000200001835018350183501335013350103401031010310000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
19060c00184501d4401f44024430184201d4201f41024410184001d4001f400244000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011010000c550005000c5500050013550005001155013550145500050011550005001055000500135501455000000000000000000000000000000000000000000000000000000000000000000000000000000000
010800001c753000030000000000000000000000000000003c63500000000000000000000000000000000000000000000000000000001c7530000000000000003c63500000000000000000000000000000000000
001000000c120001200c1200012011120051201112005120101200412010120101200412010120141201412000000000000000000000000000000000000000000000000000000000000000000000000000000000
48020020056100e610046100361011610036100561005610066101061006610076100761008610086100861013610076100761008610086100861013610076100961009610096100961013610076100661006610
__music__
03 41420407
03 41050407

