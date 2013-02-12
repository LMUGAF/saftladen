import os
from pwd import getpwnam
from grp import getgrnam


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
