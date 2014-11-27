#!/usr/bin/python3

import requests
import json
import random


class SlUser:
	id      = None
	name    = None
	mail    = None
	gafAcc  = None
	irc     = None
	total   = 0
	note    = None
	enabled = True
	
	def __init__(self, sl, data = None):
		self.sl = sl
		
		if data is not None:
			self.fromSet(data)
	
	
	
	def fromSet(self, data):
		self.id      = int(data['id'])
		self.name    = data['name']
		self.mail    = data['mail']
		self.gafAcc  = data['gaf_acc']
		self.irc     = data['irc']
		self.total   = int(data['total'])
		self.note    = data['note']
		self.enabled = data['enabled'] == '1'
	
	
	
	def toSet(self):
		data = {}
		
		if self.id is not None:
			data['id']      = "%i" % self.id
		
		data['name']    = self.name
		data['mail']    = self.mail
		data['gaf_acc'] = self.gafAcc
		data['irc']     = self.irc
		
		if self.total is not None:
			data['total']   = "%i" % self.total
		
		data['note']    = self.note
		
		if self.enabled == True:
			data['enabled'] = '1'
		elif self.enabled == False:
			data['enabled'] = '0'
		
		
		return data
	
	
	
	def __str__(self):
		return "User '%s' (%i) with a total of %0.2f₲ (Mail: %s | IRC: %s)" % \
			(self.name, self.id, self.total/100, self.mail, self.irc)


class SlProduct:
	id          = None
	name        = None
	ean         = None
	price       = None
	amount      = None
	volume      = None
	caffeine    = None
	alcohol     = None
	note        = None
	description = None
	
	
	def __init__(self, sl, data = None):
		self.sl = sl
		
		if data is not None:
			self.fromSet(data)
	
	
	def fromSet(self, data):
		self.id           = int(data['id'])
		self.name        = data['name']
		self.ean         = data['ean']
		self.price       = int(data['price'])
		self.amount      = int(data['amount'])
		
		if data['volume'] is None:
			self.volume  = None
		else:
			self.volume  = int(data['volume'])
		
		self.caffeine    = int(data['caffeine'])
		self.alcohol     = float(data['alcohol'])
		self.note        = data['note']
		self.description = data['description']
	
	
	def toSet(self):
		data = {}
		
		if self.id is not None:
			data['id']      = "%i" % self.id
		
		data['name']        = self.name
		data['ean']         = self.ean
		data['price']       = self.price
		data['amount']      = self.amount
		data['volume']      = self.volume
		data['caffeine']    = self.caffeine
		data['alcohol']     = self.alcohol
		data['note']        = self.note
		data['description'] = self.description
		
		return data
	
	
	def __str__(self):
		return "Product %s (EAN %s) consts %0.2f₲. %i remaining." % \
			(self.name, self.ean, self.price/100, self.amount)


class SlTransaction:
	def __init__(self, sl, data = None):
		self.sl = sl
		
		if data is not None:
			self.fromSet(data)
	
	
	id            = None
	productId     = None
	userId        = None
	transactionId = None
	type          = None
	price         = None
	date          = None
	origin        = None
	
	
	def fromSet(self, data):
		self.id            = int(data['id'])
		
		if data['product_id'] is None:
			self.productId = None
		else:
			self.productId = int(data['product_id'])
		
		self.userId        = int(data['user_id'])
		
		if data['transaction_id'] is None:
			self.transactionId = None
		else:
			self.transactionId = int(data['transaction_id'])
			
		self.type          = data['type']
		self.price         = int(data['price'])
		self.date          = data['date']
		self.origin        = data['origin']
	
	
	def toSet(self):
		data = {}
		
		if self.id is not None:
			data['id']      = "%i" % self.id
			
		data['product_id']     = self.productId
		data['user_id']        = self.userId
		data['transaction_id'] = self.transactionId
		data['type']          = self.type
		data['price']         = self.price
		data['date']          = self.date
		data['origin']        = self.origin
		
		return data
	
	def __str__(self):
		return "Transaction from %s. UID %i %s product %s / transaction %s worth %0.2f₲ via %s." % \
			(self.date, self.userId, self.type, self.productId, self.transactionId, self.price/100, self.origin)


class Sl:
	pageSize = 100
	
	def __init__(self, baseUrl):
		self.baseUrl = baseUrl
	
	def getBaseUrl(self):
		return self.baseUrl
	
	
	## === X ===================================================================
	def __x(self, urlPart, cls):
		url = "%s/%s" % (self.baseUrl, urlPart)
		headers = {'Content-type': 'application/json'}
		r = requests.get(url, headers=headers)
		
		if r.text == "null":
			raise Exception("No such resource")
		
		return cls(self, r.json())
	
	
	
	def __xs(self, urlPart, cls):
		more = True
		page = 0
		
		while more:
			url = "%s/%s" % (self.baseUrl, urlPart)
			params = {'max': self.pageSize, 'offset': self.pageSize*page}
			headers = {'Content-type': 'application/json'}
			r = requests.get(url, params=params, headers=headers)
			#print(r.text)
			data = r.json()
			more = data['hasmore']
			
			for item in data['items']:
				yield cls(self, item)
			
			page += 1
	
	
	
	def __saveX(self, urlPart, x):
		if x.id is None:
			url = "%s/%s" % (self.baseUrl, urlPart)
			data = x.toSet()
			headers = {'Content-type': 'application/json'}
			r = requests.post(url, data=json.dumps(data), headers=headers)
		else:
			url = "%s/%s/id/%i" % (self.baseUrl, urlPart, x.id)
			data = x.toSet()
			headers = {'Content-type': 'application/json'}
			r = requests.put(url, data=json.dumps(data), headers=headers)
		
		if r.text != "null":
			print(r.text)
			raise Exception("Save failed")
	
	
	## === User ================================================================
	def userById(self, uid):
		return self.__x("users/id/%i" % uid, SlUser)
	
	
	def userByName(self, name):
		return self.__x("users/name/%s" % name, SlUser)
	
	
	def users(self):
		for x in self.__xs("users", SlUser):
			yield x
	
	
	def saveUser(self, user):
		self.__saveX("users", user)
	
	
	## === Product =============================================================
	def productById(self, pid):
		return self.__x("products/id/%i" % pid, SlProduct)
	
	
	def productByName(self, name):
		return self.__x("products/name/%s" % name, SlProduct)
	
	
	def productByEan(self, ean):
		return self.__x("products/ean/%s" % ean, SlProduct)
	
	
	def products(self):
		for x in self.__xs("products", SlProduct):
			yield x
	
	
	def saveProduct(self, product):
		self.__saveX("products", product)
	
	
	## === Product =============================================================
	def transactionById(self, tid):
		return self.__x("transactions/id/%i" % tid, SlTransaction)
	
	
	def transactions(self):
		for x in self.__xs("transactions", SlTransaction):
			yield x
	
	
	def saveTransaction(self, transaction):
		self.__saveX("transactions", transaction)
	
	
	

		

#sl = Sl("https://gaf.fs.lmu.de/saftladen/skruppy/index.php")

#u = sl.userById(2)
#print(u)

##u.total = random.randint(0, 100)
##u.mail = None ### @todo Not Working
##sl.saveUser(u)



###u = sl.userById(2)
###print(u)

###u = SlUser(sl)
###sl.saveUser(u)

##for u in sl.users():
	##print(u)

##for u in sl.products():
	##print(u)

#p = sl.productByEan("5000159407359")
#print(p)

#x = SlTransaction(sl)
#x.type = "buy"
#x.userId = u.id
#x.productId = p.id
#x.origin = "slsd"
#x.price = p.price
#sl.saveTransaction(x)

#for u in sl.transactions():
	#print(u)

#u = sl.userByName("sdfs")
#print(u)
