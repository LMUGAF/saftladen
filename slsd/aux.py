import os
from pwd import getpwnam
from grp import getgrnam
import threading



def setFileAccess(path, mode = None, user = None, group = None):
	#### Mode
	if mode is not None:
		os.chmod(path, int(mode, 8))
	
	#### User
	if user is not None:
		user = str(user)
		## If not already a number reslov name
		if user.isdigit() == False:
			user = getpwnam(user).pw_uid
		user = int(user)
	
	## No change of the user: -1
	else:
		user = -1
	
	#### Group
	if group is not None:
		group = str(group)
		## If not already a number reslov name
		if group.isdigit() == False:
			group = getgrnam(group).gr_gid
		group = int(group)
	
	## No change of the group: -1
	else:
		group = -1
	
	#### Actually change user and group
	os.chown(path, user, group)


## http://www.theorangeduck.com/page/synchronized-python
def synchronized(func):
	func.__lock__ = threading.Lock()
	
	def synced_func(*args, **kws):
		with func.__lock__:
			return func(*args, **kws)
	
	return synced_func


def synchronized_method(method):
	outer_lock = threading.Lock()
	lock_name = "__"+method.__name__+"_lock"+"__"
	
	def sync_method(self, *args, **kws):
		with outer_lock:
			if not hasattr(self, lock_name):
				setattr(self, lock_name, threading.Lock())
			
			lock = getattr(self, lock_name)
			with lock:
				return method(self, *args, **kws)  

	return sync_method




