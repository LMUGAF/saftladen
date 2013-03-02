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
			self.start()
			self.__running = True
	
	
	def removeListener(self, listener):
		self.__listeners.remove(listener)
		
		if len(self.__listeners) == 0:
			self.stop()
	
	
	def _notifyListeners(self, code):
		scan = Scan(self, code)
		
		for listener in self.__listeners:
			listener.scanned(scan)
	
	
	def __stopAction(self):
		pass
	
	
	def stop(self):
		self.__running = False
		self.__stopAction()




