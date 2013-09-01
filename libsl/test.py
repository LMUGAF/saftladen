#!/usr/bin/python

import requests
import json


class SlUser:
	gafAcc  = None
	name    = None
	mail    = None
	enabled = True
	total   = 0
	id      = None
	note    = None
	irc     = None
	
	def __init__(self, sl, data = None):
		self.sl = sl
		
		if data is not None:
			self.fromSet(data)
	
	
	
	def fromSet(self, data):
		self.gafAcc  = data['gaf_acc']
		self.name    = data['name']
		self.mail    = data['mail']
		self.enabled = data['enabled'] == '1'
		self.total   = float(data['total'])
		self.id      = int(data['id'])
		self.note    = data['note']
		self.irc     = data['irc']
	
	
	
	def toSet(self):
		data = {}
		
		data['gaf_acc'] = self.gafAcc
		data['name']    = self.name
		data['mail']    = self.mail
		if self.enabled == True:
			data['enabled'] = '1'
		elif self.enabled == False:
			data['enabled'] = '0'
		
		if self.total is not None:
			data['total']   = "%f" % self.total
		
		if self.id is not None:
			data['id']      = "%i" % self.id
		
		data['note']    = self.note
		data['irc']     = self.irc
		
		return data
	
	
	
	def __str__(self):
		return "User '%s' (%i) with a total of %f (Mail: %s | IRC: %s)" % \
			(self.name, self.id, self.total, self.mail, self.irc)
	
	
class Sl:
	def __init__(self, baseUrl):
		self.baseUrl = baseUrl
	
	def getBaseUrl(self):
		return self.baseUrl
	
	def getUserById(self, uid):
		url = "%s/users/id/%i" % (self.baseUrl, uid)
		headers = {'Content-type': 'application/json'}
		r = requests.get(url, headers=headers)
		return SlUser(self, r.json())
	
	
	def getUsers(self):
		more = True
		page = 0
		
		while more:
			url = "%s/users" % self.baseUrl
			params = {'max': 10, 'offset': 10*page}
			headers = {'Content-type': 'application/json'}
			data = requests.get(url, params=params, headers=headers).json()
			
			more = data['hasmore']
			
			for item in data['items']:
				yield SlUser(self, item)
			
			page += 1
	
	
	
	def saveUser(self, user):
		if user.id is None:
			url = "%s/users" % self.baseUrl
			data = user.toSet()
			headers = {'Content-type': 'application/json'}
			r = requests.post(url, data=json.dumps(data), headers=headers)
			print(r.text)
		else:
			url = "%s/users/id/%i" % (self.baseUrl, user.id)
			data = user.toSet()
			headers = {'Content-type': 'application/json'}
			r = requests.put(url, data=json.dumps(data), headers=headers)
		

sl = Sl("https://gaf.fs.lmu.de/saftladen/skruppy/index.php")

#u = sl.getUserById(2)
#print(u)

#u.total = 42
#u.mail = None ### @todo Not Working

#sl.saveUser(u)

#u = sl.getUserById(2)
#print(u)

#u = SlUser(sl)
#sl.saveUser(u)

for u in sl.getUsers():
	print(u)

