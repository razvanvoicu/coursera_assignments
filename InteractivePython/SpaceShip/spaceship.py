# http://www.codeskulptor.org/#user40_YaPYF0j0nuNI13u.py
import simplegui
import math
import random

# globals for user interface
WIDTH = 800
HEIGHT = 600
ANGULAR_SPEED = 0.1
SPEED_LIMIT = 10.0
FRICTION = 0.99
MISSILE_SPEED = 20.0
score = 0
lives = 3
time = 0
ship_rotation = 0

class ImageInfo:
    def __init__(self, center, size, radius = 0, lifespan = None, animated = False):
        self.center = center
        self.size = size
        self.radius = radius
        if lifespan:
            self.lifespan = lifespan
        else:
            self.lifespan = float('inf')
        self.animated = animated

    def get_center(self):
        return self.center

    def get_size(self):
        return self.size

    def get_radius(self):
        return self.radius

    def get_lifespan(self):
        return self.lifespan

    def get_animated(self):
        return self.animated

    
# art assets created by Kim Lathrop, may be freely re-used in non-commercial projects, please credit Kim
    
# debris images - debris1_brown.png, debris2_brown.png, debris3_brown.png, debris4_brown.png
#                 debris1_blue.png, debris2_blue.png, debris3_blue.png, debris4_blue.png, debris_blend.png
debris_info = ImageInfo([320, 240], [640, 480])
debris_image = simplegui.load_image("http://commondatastorage.googleapis.com/codeskulptor-assets/lathrop/debris2_blue.png")

# nebula images - nebula_brown.png, nebula_blue.png
nebula_info = ImageInfo([400, 300], [800, 600])
nebula_image = simplegui.load_image("http://commondatastorage.googleapis.com/codeskulptor-assets/lathrop/nebula_blue.f2014.png")

# splash image
splash_info = ImageInfo([200, 150], [400, 300])
splash_image = simplegui.load_image("http://commondatastorage.googleapis.com/codeskulptor-assets/lathrop/splash.png")

# ship image
ship_info = ImageInfo([45, 45], [90, 90], 20)
ship_image = simplegui.load_image("http://commondatastorage.googleapis.com/codeskulptor-assets/lathrop/double_ship.png")

# missile image - shot1.png, shot2.png, shot3.png
missile_info = ImageInfo([5,5], [10, 10], 3, 50)
missile_image = simplegui.load_image("http://commondatastorage.googleapis.com/codeskulptor-assets/lathrop/shot2.png")

# asteroid images - asteroid_blue.png, asteroid_brown.png, asteroid_blend.png
asteroid_info = ImageInfo([45, 45], [90, 90], 40)
asteroid_image = simplegui.load_image("http://commondatastorage.googleapis.com/codeskulptor-assets/lathrop/asteroid_blue.png")

# animated explosion - explosion_orange.png, explosion_blue.png, explosion_blue2.png, explosion_alpha.png
explosion_info = ImageInfo([64, 64], [128, 128], 17, 24, True)
explosion_image = simplegui.load_image("http://commondatastorage.googleapis.com/codeskulptor-assets/lathrop/explosion_alpha.png")

# sound assets purchased from sounddogs.com, please do not redistribute
soundtrack = simplegui.load_sound("http://commondatastorage.googleapis.com/codeskulptor-assets/sounddogs/soundtrack.mp3")
missile_sound = simplegui.load_sound("http://commondatastorage.googleapis.com/codeskulptor-assets/sounddogs/missile.mp3")
missile_sound.set_volume(.5)
ship_thrust_sound = simplegui.load_sound("http://commondatastorage.googleapis.com/codeskulptor-assets/sounddogs/thrust.mp3")
explosion_sound = simplegui.load_sound("http://commondatastorage.googleapis.com/codeskulptor-assets/sounddogs/explosion.mp3")

# helper functions to handle transformations
def angle_to_vector(ang):
    return [math.cos(ang), math.sin(ang)]

def dist(p,q):
    return math.sqrt((p[0] - q[0]) ** 2+(p[1] - q[1]) ** 2)


# Ship class
class Ship:
    def __init__(self, pos, vel, angle, image, info):
        self.pos = [pos[0],pos[1]]
        self.vel = [vel[0],vel[1]]
        self.thrust = False
        self.angle = angle
        self.angle_vel = 0
        self.image = image
        self.image_center = info.get_center()
        self.image_size = info.get_size()
        self.radius = info.get_radius()
        
    def draw(self,canvas):
        thrust = 0
        if self.thrust: thrust = 1
        center = ship_info.get_center()
        size = ship_info.get_size()
        canvas.draw_image(
            self.image,
            [center[0]+thrust*size[0],center[1]],
            size, 
            self.pos,
            size,
            self.angle
        )

    def update(self):
        self.angle += ANGULAR_SPEED * ship_rotation
        if self.thrust:
            angvect = angle_to_vector(self.angle)
            self.vel = [self.vel[0] + angvect[0], self.vel[1] + angvect[1]]
            velmag = math.sqrt(self.vel[0]*self.vel[0] + self.vel[1]*self.vel[1])
            if velmag > SPEED_LIMIT:
                self.vel = [self.vel[0]*SPEED_LIMIT/velmag,self.vel[1]*SPEED_LIMIT/velmag]
        else:
            self.vel = [self.vel[0]*FRICTION,self.vel[1]*FRICTION]
        self.pos = [self.pos[0]+self.vel[0], self.pos[1]+self.vel[1]]
        if self.pos[0] > WIDTH: self.pos[0] -= WIDTH
        if self.pos[1] > HEIGHT: self.pos[1] -= HEIGHT
        if self.pos[0] < 0 : self.pos[0] += WIDTH
        if self.pos[1] < 0 : self.pos[1] += HEIGHT
        if self.thrust:
            ship_thrust_sound.play()
        else:
            ship_thrust_sound.pause()
    
    
# Sprite class
class Sprite:
    def __init__(self, pos, vel, ang, ang_vel, image, info, sound = None):
        self.pos = [pos[0],pos[1]]
        self.vel = [vel[0],vel[1]]
        self.angle = ang
        self.angle_vel = ang_vel
        self.image = image
        self.image_center = info.get_center()
        self.image_size = info.get_size()
        self.radius = info.get_radius()
        self.lifespan = info.get_lifespan()
        self.animated = info.get_animated()
        self.age = 0
        if sound:
            sound.rewind()
            sound.play()
   
    def draw(self, canvas):
        center = self.image_center
        size = self.image_size
        canvas.draw_image(
            self.image,
            center,
            size, 
            self.pos,
            size,
            self.angle
        )
    
    def update(self):
        self.angle += ANGULAR_SPEED * self.angle_vel
        angvect = angle_to_vector(self.angle)
        self.pos = [self.pos[0]+self.vel[0], self.pos[1]+self.vel[1]]
        if self.pos[0] > WIDTH: self.pos[0] -= WIDTH
        if self.pos[1] > HEIGHT: self.pos[1] -= HEIGHT
        if self.pos[0] < 0 : self.pos[0] += WIDTH
        if self.pos[1] < 0 : self.pos[1] += HEIGHT

           
def draw(canvas):
    global time
    
    # animiate background
    time += 1
    wtime = (time / 4) % WIDTH
    center = debris_info.get_center()
    size = debris_info.get_size()
    canvas.draw_image(nebula_image, nebula_info.get_center(), nebula_info.get_size(), [WIDTH / 2, HEIGHT / 2], [WIDTH, HEIGHT])
    canvas.draw_image(debris_image, center, size, (wtime - WIDTH / 2, HEIGHT / 2), (WIDTH, HEIGHT))
    canvas.draw_image(debris_image, center, size, (wtime + WIDTH / 2, HEIGHT / 2), (WIDTH, HEIGHT))

    canvas.draw_text("Score",[11*WIDTH/12,HEIGHT/24],20,"yellow")
    canvas.draw_text(str(score),[11*WIDTH/12,1.8*HEIGHT/24],20,"yellow")
    canvas.draw_text("Lives",[WIDTH/72,HEIGHT/24],20,"yellow")
    canvas.draw_text(str(lives),[WIDTH/72,1.8*HEIGHT/24],20,"yellow")

    # draw ship and sprites
    my_ship.draw(canvas)
    a_rock.draw(canvas)
    if a_missile:
        a_missile.draw(canvas)
    
    # update ship and sprites
    my_ship.update()
    a_rock.update()
    if a_missile:
        a_missile.update()

def keydown(key):
    global ship_rotation, ship_thrust, a_missile
    if key == simplegui.KEY_MAP["left"]:
        ship_rotation = -1
    elif key == simplegui.KEY_MAP["right"]:
        ship_rotation = 1
    elif key == simplegui.KEY_MAP["up"]:
        my_ship.thrust = True
    elif key == simplegui.KEY_MAP["space"]:
        vec = angle_to_vector(my_ship.angle)
        pos = [ my_ship.pos[0]+my_ship.radius*vec[0], 
                my_ship.pos[1]+my_ship.radius*vec[1] ]
        a_missile = Sprite(
            [pos[0]+vec[0]*my_ship.radius,pos[1]+vec[1]*my_ship.radius], 
            [MISSILE_SPEED*vec[0]+my_ship.vel[0],MISSILE_SPEED*vec[1]+my_ship.vel[1]], 
            0, 0, missile_image, missile_info, missile_sound
        )
    else:
        pass


def keyup(key):
    global ship_rotation, ship_thrust
    if key == simplegui.KEY_MAP["left"]:
        ship_rotation = 0
    elif key == simplegui.KEY_MAP["right"]:
        ship_rotation = 0
    elif key == simplegui.KEY_MAP["up"]:
        my_ship.thrust = False
    else:
        pass
    
# timer handler that spawns a rock    
def rock_spawner():
    global a_rock, a_missile
    a_rock = Sprite([WIDTH*random.random(), HEIGHT*random.random()], 
                    [-0.5+random.random(), -0.5*random.random()], 
                    -3.14+6.28*random.random(), 
                    -1+2*random.random(), 
                    asteroid_image, asteroid_info)

    
# initialize frame
frame = simplegui.create_frame("Asteroids", WIDTH, HEIGHT)

# initialize ship and two sprites
my_ship = Ship([WIDTH / 2, HEIGHT / 2], [0, 0], 0, ship_image, ship_info)
a_rock = Sprite([WIDTH*random.random(), HEIGHT*random.random()], 
                    [-0.5+random.random(), -0.5*random.random()], 
                    -3.14+6.28*random.random(), 
                    -1+2*random.random(), 
                    asteroid_image, asteroid_info)
a_missile = None

# register handlers
frame.set_draw_handler(draw)
frame.set_keydown_handler(keydown)
frame.set_keyup_handler(keyup)

timer = simplegui.create_timer(1000.0, rock_spawner)

# get things rolling
timer.start()
frame.start()
