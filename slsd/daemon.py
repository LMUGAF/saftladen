## http://www.enderunix.org/docs/eng/daemon.php
## http://www.netzmafia.de/skripten/unix/linux-daemon-howto.html

## http://sourceware.org/git/?p=glibc.git;a=blob;f=misc/daemon.c#

## http://www.win.tue.nl/~aeb/linux/lk/lk-10.html

## https://github.com/rfc83/turkmenbashi-python/blob/master/turkmenbashi/daemon_creator.py


import os
import sys
import threading
from pwd import getpwnam
from grp import getgrnam
import aux



class Daemon:
	def __init__(self):
		self.stdin = None
		self.stdout = None
		self.stderr = None
		self.cwd = "/"
		self.user = None
		self.group = None
		self.pidbefore = True
		self.pidmode = None
		self.piduser = None
		self.pidgroup = None
		self.pidfile = None
	
	
	def daemonize(self):
		threadCnt = threading.activeCount()
		if threadCnt > 1:
			raise Exception("Can't daemonize since there are %i running threads which wouldn't be forked." % threadCnt)
		
		## Fork
		pid = os.fork()
		if pid > 0:
			if self.pidbefore == True:
				self.writePid(pid)
			sys.exit(0)
		
		if self.pidbefore == False:
			self.writePid(os.getpid())
		
		
		os.setsid()
		
		## Change CWD
		self.setCwd(self.cwd)
		self.setGid(self.group)
		self.setUid(self.user)
		
		
		## Redirect STDIO
		self.redirectStdIo(sys.stdin, self.stdin)
		self.redirectStdIo(sys.stdout, self.stdout)
		self.redirectStdIo(sys.stderr, self.stderr)
	
	
	def writePid(self, pid):
		if self.pidfile is not None:
			file = open(self.pidfile, "w+")
			file.write(str(pid))
			file.close()
			aux.setFileAccess(self.pidfile, self.pidmode, self.piduser, self.pidgroup)
		
	
	
	def setCwd(self, path):
		if path is not None:
			os.chdir(path)
	
	
	def setUid(self, user):
		if user is not None:
			if user.isdigit() == False:
				user = getpwnam(user).pw_uid
			
			os.setuid(user)
	
	
	def setGid(self, group):
		if group is not None:
			if group.isdigit() == False:
				group = getgrnam(group).gr_gid
			
			os.setgid(group)
	
	
	def redirectStdIo(self, old, new):
		if new is None:
			if hasattr(os, "devnull"):
				nullPath = os.devnull
			else:
				nullPath = "/dev/null"
			
			newFd = os.open(nullPath, os.O_RDWR)
		else:
			newFd = new.fileno()
		
		old.flush() ## this is an extra
		os.dup2(newFd, old.fileno())
		
		if newFd > 2:
			if new is None:
				os.close(newFd)
			else:
				pass
				#new.close()
