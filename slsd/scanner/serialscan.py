import conf
import serial
import glob
from scan import *
from route import ERoutingResult

class SerialScanner(Scanner, conf.ScannerPlugin):
	elName = "serialscanner"
	
	def run(self):
		self.__ser = serial.Serial(glob.glob('/dev/ttyUSB0')[0], 9600, timeout=1, stopbits=1)
		
		while self._shouldRun():
			try:
				byteChar = self.__ser.readline()
			except serial.SerialException:
				self.handler.portDisconnect()
				pass
			
			if len(byteChar) > 0:
				self._notifyListeners(byteChar[:-1].decode('UTF-8'))
	
	
	def _stopAction(self):
		if self.__ser is not None:
			self.__ser.close()
			self.__ser = None
	
	def create(el, name):
		handler = SerialScanner(name)
		
		return handler
