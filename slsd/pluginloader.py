import os
import sys
import imp
from log import Log


def loadPlugins(name):
	path = os.path.realpath(__file__)
	path = os.path.dirname(path)
	path = os.path.join(path, name)
	
	if not path in sys.path:
		sys.path.append(path)
	
	for f in os.listdir(path):
		if f.endswith(".py"):
			pluginPath = os.path.join(path, f)
			pluginName = "%s_%s_plugin" % (name, f[:-3])
			
			mod = imp.load_source(pluginName, pluginPath)
			#mod.init()
			
			Log.log(
				Log.LVL_INFO, Log.SRV_MISC,
				"Loading plugin %s as \"%s\"" % (pluginPath, pluginName)
			)
