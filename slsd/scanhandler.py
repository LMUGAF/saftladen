from log import Log
import mpd
from route import ERoutingResult
from sh import sh


class DebugScanhandler:
	def __init__(self):
		self.resetPort()
	
	
	def __trap(self, num, code):
		self.__cnt[num] += 1
		Log.log(
			Log.LVL_DEBUG2, Log.SRV_DEB_H,
			"Debug port %i triggered with \"%s\"the %ith time" %
			(num, code, self.__cnt[num])
		)
		return ERoutingResult.STOP_OK
	
	
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
		return ERoutingResult.STOP_OK



class MpdScanhandler:
	def __init__(self, hostname, port = 6600):
		self.__client = mpd.MPDClient()
		self.__client.timeout = 10
		self.__client.idletimeout = None
		self.__client.connect(hostname, port)
	
	
	def pausePlayPort(self, code):
		status = self.__client.status()['state']
		if status == "play" or status == "pause":
			self.__client.pause()
		else:
			self.__client.play()
		return ERoutingResult.STOP_OK
	
	
	def nextPort(self, code):
		self.__client.next()
		return ERoutingResult.STOP_OK



class ExecScanhandler:
	def __init__(self, cmd, timeout = 10):
		self.__cmd = cmd
		self.__timeout = timeout
	
	def execPort(self, code):
		sh(self.__cmd, self.__timeout)
		return ERoutingResult.STOP_OK

