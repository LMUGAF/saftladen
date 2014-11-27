import conf
from log import Log
from route import ERoutingResult
import threading
import queue
import libsl


class SlMsg:
	action = ""
	code = ""
	
	
	def __init__(self, action, code):
		self.action = action
		self.code = code



class SlThread(threading.Thread):
	reuseMsg = None
	
	
	def __init__(self, url, queue):
		super().__init__()
		self.url = url
		self.queue = queue
	
	
	def run(self):
		Log.log(
			Log.LVL_DEBUG2, Log.SRV_DEB_H,
			"Saftladen worker started %s" % self.url
		)
		self.sl = libsl.Sl(self.url)
		
		while True:
			try:
				self.step1()
			except Exception as ex:
				Log.log(
					Log.LVL_ERROR, Log.SRV_DEB_H,
					"Something went wrong in sl: %s" % ex
				)
	
	
	
	def step1(self):
		if self.reuseMsg is None:
			msg = self.queue.get()
		else:
			msg = self.reuseMsg
			self.reuseMsg = None
		
		
		## ------ User by ID ---------------------------------------------------
		if msg.action == "userById":
			try:
				user = self.sl.userById(msg.code)
			except:
				Log.log(
					Log.LVL_DISPLAY, Log.SRV_DEB_H,
					"No such user (UID %i)" % msg.code
				)
				return
		
		
		## ------ User by name -------------------------------------------------
		elif msg.action == "userByName":
			try:
				user = self.sl.userByName(msg.code)
			except:
				Log.log(
					Log.LVL_DISPLAY, Log.SRV_DEB_H,
					"No such user (named %s)" % msg.code
				)
				return
		
		
		## ------ Wrong action -------------------------------------------------
		else:
			Log.log(
				Log.LVL_DISPLAY, Log.SRV_DEB_H,
				"Nice scan (%s, %s), but not now!" % (msg.action, msg.code)
			)
			return
		
		Log.log(
			Log.LVL_DISPLAY, Log.SRV_DEB_H,
			"Hi %s, you have %0.2f₲" % (user.name, user.total/100)
		)
		
		
		
		while self.step2(user):
			pass
		
		
		Log.log(
			Log.LVL_DISPLAY, Log.SRV_DEB_H,
			"Bye %s" % user.name
		)
	
	
	
	def step2(self, user):
		try:
			msg = self.queue.get(True, 5)
		except queue.Empty:
			return False
		
		
		## ------ Buy ----------------------------------------------------------
		if msg.action == "productByEan":
			try:
				p = self.sl.productByEan(msg.code)
			except:
				Log.log(
					Log.LVL_DISPLAY, Log.SRV_DEB_H,
					"Product %s is not in the system" % (msg.code)
				)
				return True
			
			x = libsl.SlTransaction(self.sl)
			
			x.type = "buy"
			x.userId = user.id
			x.productId = p.id
			x.origin = "slsd"
			x.price = p.price
			
			self.sl.saveTransaction(x)
			
			Log.log(
				Log.LVL_DISPLAY, Log.SRV_DEB_H,
				"You bought %s for %0.2f₲" % (p.name, p.price/100)
			)
			
			return True
		
		
		## ------ Deposit/withdraw ---------------------------------------------
		elif msg.action == "deposit":
			x = libsl.SlTransaction(self.sl)
			
			x.type = "in"
			x.userId = user.id
			x.origin = "slsd"
			x.price = -msg.code
			
			self.sl.saveTransaction(x)
			
			Log.log(
				Log.LVL_DISPLAY, Log.SRV_DEB_H,
				"Now you have %0.2f₲ %s" % (abs(msg.code)/100, "more" if msg.code > 0 else "less")
			)
			
			return True
		
		
		## ------ Logout -------------------------------------------------------
		elif msg.action == "logout":
			return False
		
		
		## ------ Switch user --------------------------------------------------
		elif msg.action == "userById" or msg.action == "userByName":
			self.reuseMsg = msg
			return False
		
		
		## ------ Wrong action -------------------------------------------------
		else:
			Log.log(
				Log.LVL_DISPLAY, Log.SRV_DEB_H,
				"Nice scan (%s, %s), but not now!" % (msg.action, msg.code)
			)
			return True





class SlScanhandler(conf.HandlerPlugin):
	elName = "slhandler"
	
	
	def __init__(self, url):
		self.queue = queue.Queue()
		t = SlThread(url, self.queue)
		t.start()
	
	
	
	def __send(self, code, action):
		Log.log(
			Log.LVL_DEBUG2, Log.SRV_DEB_H,
			"Sending %s: %s to sl" % (action, code)
		)
		
		self.queue.put(SlMsg(action, code))
		
		return ERoutingResult.STOP_OK
	
	
	def getPort(self, name):
		if   name == "userById":     return self.userByIdPort
		elif name == "userByName":   return self.userByNamePort
		elif name == "productByEan": return self.productByEanPort
		elif name == "productByNr":  return self.productByNrPort
		elif name == "storno":       return self.stornoPort
		elif name == "withdraw":     return self.withdrawPort
		elif name == "deposit":      return self.depositPort
		elif name == "logout":       return self.logoutPort
		else: raise Exception("DebugScanhandler has no port %s" % name)
	
	
	def userByIdPort(self, code):
		return self.__send(int(code), "userById")
	
	
	def userByNamePort(self, code):
		return self.__send(code, "userByName")
	
	
	def productByEanPort(self, code):
		return self.__send(code, "productByEan")
	
	
	def productByNrPort(self, code):
		## Not implemented
		return ERoutingResult.STOP_FAIL
	
	
	def stornoPort(self, code):
		if code != "":
			return ERoutingResult.STOP_FAIL
		
		return self.__send("", "storno")
	
	
	def withdrawPort(self, code):
		return self.__send(-int(code), "deposit")
	
	
	def depositPort(self, code):
		return self.__send(int(code), "deposit")
	
	
	def logoutPort(self, code):
		if code != "":
			return ERoutingResult.STOP_FAIL
		
		return self.__send("", "logout")
	
	
	def create(el, name):
		handler = SlScanhandler(el.get("url"))
		return handler
	
testCases = """
uid 2
logout

uid 2
4072700001126
uid 1
4072700001126
uid 2
uid 1

uid 2
4072700001126
logout

uid 2
deposit 1000
logout

uid 2
withdraw 1000
logout

"""