#!/usr/bin/python3

import sys
import pygame
import os
import select
import fcntl
import logging
import errno
import collections


class NamedPipeLineReader(object):
	def __init__(self, path):
		self.__path  = path
	
	
	def waitLine(self, timeoutCallback):
		remainingData = ""
		
		for data in self.__waitData(timeoutCallback):
			if data is None:
				yield None
			
			else:
				if remainingData is not None:
					lines = (remainingData + data).split("\n")
					## Only the first lines gets data (without an "\n")
					## prefixed, therefore only the first line can become to big
					if len(lines[0]) > 511:
						logging.info("Discarding oversized line '%s'", lines[0])
						lines = lines[1:]
				
				## We have no valid data from the last time, indicating an error
				## has occurred. Therefore the first "line" still belongs to the
				## bogus data and has to be discarded.
				else:
					lines = data.split("\n")[1:]
				
				## Now lines contains only valid or no line (The last "line" is
				## not terminated and belongs to the next data)
				
				if len(lines) > 0:
					## Save the last line for the next round
					remainingData = lines[-1]
					
					## Return all other lines (all except for the last one)
					for line in lines[:-1]:
						yield line
				
				else:
					remainingData = None
	
	
	def __waitData(self, timeoutCallback):
		## Open the reading end of the named pipe as non-blocking
		## The "+" causes the file to stay open if the other side disconnects
		f = open(self.__path, "rb+", buffering=0 )
		fd = f.fileno()
		fcntl.fcntl(fd, fcntl.F_SETFL, fcntl.fcntl(fd, fcntl.F_GETFL) | os.O_NONBLOCK)
		
		## Purge old data out of the named pipe
		while True:
			try:
				logging.warning(
					"Discarding old data from named pipe '%s'",
					os.read(fd, 1024)
				)
			except OSError as ex:
				if ex.errno == errno.EAGAIN:
					break
				else:
					raise ex
		
		## Setup level triggered polling
		epoll = select.epoll()
		epoll.register(fd, select.EPOLLIN)
		
		## Poll to infinity
		while True:
			events = epoll.poll(timeoutCallback(), 1)
			
			if len(events) == 0:
				yield None
			else:
				yield os.read(fd, 512).decode('UTF-8')


def displayText(screen, font, color, lineSep, lines, startPos):
	for line in lines:
		rendered = font.render(line, 1, color)
		screen.blit(rendered, startPos)
		startPos[1] += lineSep

#pygame.init()

def initDisplay():
	## Try X11 first
	if os.getenv("DISPLAY"):
		pygame.display.init()
		return
	
	## Then frame buffer
	drivers = ['directfb', 'fbcon', 'svgalib']
	
	for driver in drivers:
		if not os.getenv('SDL_VIDEODRIVER'):
			os.putenv('SDL_VIDEODRIVER', driver)
		try:
			pygame.display.init()
		except pygame.error:
			print('Driver: %s failed.' % format(driver))
			continue
		
		return
	
	raise Exception('No suitable video driver found!')

pygame.font.init()
initDisplay()

size = width, height = (pygame.display.Info().current_w, pygame.display.Info().current_h) #1280, 1024

screen = pygame.display.set_mode(size)
pygame.mouse.set_visible(False)

dotFont = pygame.font.Font("fonts/BPdots.otf", 28)
bg = pygame.image.load("img/bg.bmp")

ctlFile = '/run/slut/control'

ctlChannel = NamedPipeLineReader(ctlFile)

lines = collections.deque(maxlen=3)

def timeout():
	return -1

for line in ctlChannel.waitLine(timeout):
	if line == "\033[2J":
		lines.clear()
	else:
		lines.append(line[:39])
	
	screen.blit(bg, [0,0])
	displayText(screen, dotFont, (0x38, 0xe2, 0xec), 42, lines, [330,515])
	pygame.display.flip()
