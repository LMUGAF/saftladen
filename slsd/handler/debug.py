import conf
from log import Log
from route import ERoutingResult

class DebugScanhandler(conf.HandlerPlugin):
	elName = "debughandler"
	
	
	def __init__(self):
		self.resetPort()
	
	
	def __trap(self, num, code):
		self.__cnt[num] += 1
		Log.log(
			Log.LVL_DEBUG2, Log.SRV_DEB_H,
			"Debug port %i triggered with \"%s\" the %ith time" %
			(num, code, self.__cnt[num])
		)
		return ERoutingResult.STOP_OK
	
	
	def getPort(self, name):
		if   name == "trap0": return self.trap0Port
		elif name == "trap1": return self.trap1Port
		elif name == "trap2": return self.trap2Port
		elif name == "trap3": return self.trap3Port
		elif name == "trap4": return self.trap4Port
		elif name == "trap5": return self.trap5Port
		elif name == "trap6": return self.trap6Port
		elif name == "trap7": return self.trap7Port
		elif name == "trap8": return self.trap8Port
		elif name == "trap9": return self.trap9Port
		elif name == "reset": return self.resetPort
		else: raise Exception("DebugScanhandler has no port %s" % name)
	
	
	def trap0Port(self, code):
		return self.__trap(0, code)
	
	
	def trap1Port(self, code):
		return self.__trap(1, code)
	
	
	def trap2Port(self, code):
		return self.__trap(2, code)
	
	
	def trap3Port(self, code):
		return self.__trap(3, code)
	
	
	def trap4Port(self, code):
		return self.__trap(4, code)
	
	
	def trap5Port(self, code):
		return self.__trap(5, code)
	
	
	def trap6Port(self, code):
		return self.__trap(6, code)
	
	
	def trap7Port(self, code):
		return self.__trap(7, code)
	
	
	def trap8Port(self, code):
		return self.__trap(8, code)
	
	
	def trap9Port(self, code):
		return self.__trap(9, code)
	
	
	def resetPort(self, code = ""):
		self.__cnt = [0]*10
		Log.log(Log.LVL_DEBUG2, Log.SRV_DEB_H, "Resetting all debug ports")
		return ERoutingResult.STOP_OK
	
	
	def create(el, name):
		handler = DebugScanhandler()
		
		return handler