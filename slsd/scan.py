from threading import Thread
import sys


class Scan:
	def __init__(self, scanner, code):
		self.__scanner = scanner
		self.__code    = code
	
	def getScanner(self):
		return self.__scanner
	
	def getCode(self):
		return self.__code



class Scanner(Thread):
	def __init__(self, name):
		super().__init__()
		self.__name = name
		self.__listeners = []
	
	
	def addListener(self, listener):
		if not listener in self.__listeners:
			self.__listeners.append(listener)
		
		if len(self.__listeners) == 1:
			self.__running = True
			self.start()
	
	
	def removeListener(self, listener):
		self.__listeners.remove(listener)
		
		if len(self.__listeners) == 0:
			self.stop()
	
	
	def _notifyListeners(self, code):
		scan = Scan(self, code)
		
		for listener in self.__listeners:
			listener.scanned(scan)
	
	
	def _stopAction(self):
		pass
	
	
	def _shouldRun(self):
		return self.__running

