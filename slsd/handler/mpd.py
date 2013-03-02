import conf
import mpd
from route import ERoutingResult


class MpdScanhandler(conf.HandlerPlugin):
	elName = "mpdhandler"
	def __init__(self, host, service = 6600):
		self.__client = mpd.MPDClient()
		self.__client.timeout = 10
		self.__client.idletimeout = None
		self.__client.connect(host, service)
		self.__favs = {}
	
	
	def __playLast(self):
		self.__client.play()
		self.__client.seek(len(self.__client.playlistinfo()) - 1, 0)
	
	
	def getPort(self, name):
		if   name == "pausePlay":   return self.pausePlayPort
		elif name == "next":        return self.nextPort
		elif name == "enqueueFile": return self.enqueueFilePort
		elif name == "enqueueId":   return self.enqueueIdPort
		elif name == "enqueueFav":  return self.enqueueFavPort
		else: raise Exception("DebugScanhandler has no port %s" % name)
	
	
	def addFav(self, key, value):
		self.__favs[key] = value
	
	
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
	
	
	def enqueueFilePort(self, code):
		self.__client.add(code)
		self.__playLast()
		return ERoutingResult.STOP_OK
		
	
	
	def enqueueIdPort(self, code):
		self.__client.addid(code)
		self.__playLast()
		return ERoutingResult.STOP_OK
	
	
	def enqueueFavPort(self, code):
		if not code in self.__favs:
			Log.log(Log.LVL_DEBUG2, Log.SRV_MISC, "There is no favorite %s" % code)
			return ERoutingResult.STOP_OK
		
		
		if self.__favs[code].isnumeric():
			self.__client.addid(int(self.__favs[code]))
		else:
			self.__client.add(self.__favs[code])
		
		self.__playLast()
		return ERoutingResult.STOP_OK
	
	
	def create(el, name):
		if el.get("service") is None:
			handler = MpdScanhandler(el.get("host"))
		else:
			handler = MpdScanhandler(el.get("host"), el.get("service"))
		
		for element in el.findall("./fav"):
			handler.addFav(element.get("key"), element.get("value"))
		
		return handler