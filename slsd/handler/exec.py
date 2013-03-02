import conf
from sh import sh
from route import ERoutingResult


class ExecScanhandler(conf.HandlerPlugin):
	elName = "exechandler"
	
	def __init__(self, cmd, timeout = 10):
		self.setCmd(cmd)
		self.setTimeout(timeout)
	
	
	def getPort(self, name):
		if name == "exec": return self.execPort
		else: raise Exception("ExecScanhandler has no port %s" % name)
	
	
	def setTimeout(self, timeout):
		self.__timeout = timeout
	
	
	def getTimeout(self):
		return self.__timeout
	
	
	def setCmd(self, cmd):
		self.__cmd = cmd
	
	
	def getCmd(self):
		return self.__cmd
	
	
	def execPort(self, code):
		sh(self.__cmd, self.__timeout)
		return ERoutingResult.STOP_OK
	
	def create(el, name):
		handler = ExecScanhandler(el.get("cmd"))
		
		timeout = conf.intAttrib(el, "timeout")
		if timeout is not None:
			handler.setTimeout(timeout)
		
		return handler