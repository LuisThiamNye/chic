package chic.classload;

import clojure.lang.DynamicClassLoader;
// import clojure.lang.RT;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Enumeration;
import java.util.Collections;
import java.util.HashSet;
import java.io.IOException;

// Takes priority over parents
public class ComplexRootClassLoader extends URLClassLoader{

public static volatile ComplexRootClassLoader
mainLoader;

/// Priority given to smaller indices
public static volatile URLClassLoader[]
subLoaders = new URLClassLoader[0];

static {
	// Must be called before creating instances.
	// Note: all super classes must also be parallel
	var parallel = ClassLoader.registerAsParallelCapable();
	assert parallel;
	mainLoader = new ComplexRootClassLoader();
}

private ComplexRootClassLoader(){
	super(new URL[0], new DynamicClassLoader(ClassLoader.getSystemClassLoader()));
}
@Override
public URL getResource(String name){
	var r = findResource(name);
	if (r==null){
		r = this.getParent().getResource(name);
	}
	return r;
}
@Override
public Enumeration<URL> getResources(String name) throws IOException{
	var rs = findResources(name);
	if (rs==null){
		rs = this.getParent().getResources(name);
	}
	return rs;
}
@Override
public URL findResource(String name){
	URL resource;
	for (var loader: subLoaders){
		resource = loader.findResource(name);
		if (resource != null) {
			return resource;
		}
	}
	return null;
}
@Override
public Enumeration<URL> findResources(String name) throws IOException{
	var resources = new HashSet<URL>();
	for (var loader: subLoaders){
		for (var e = loader.findResources(name); e.hasMoreElements();){
			URL url = e.nextElement();
			resources.add(url);
		}
	}
	return Collections.enumeration(resources);
}
// public URL[] getPooledUrls(){
// 	var loaders = subLoaders;
// 	var n = loaders.length;
// 	var arys = new URL[n][];
// 	var ntotal = 0;
// 	for (int i=0;i<n;i++){
// 		var urls = loaders[i].getURLs();
// 		arys[i] = urls;
// 		ntotal += urls.length;
// 	}
// 	var ret = new URL[ntotal];
// 	for (int i=0;i<n;){
// 		var a = arys[i];
// 		var l = a.length;
// 		System.arraycopy(a, 0, ret, i, l);
// 		i += l;
// 	}
// 	return ret;
// }
@Override
public void close(){
	mainLoader = null;
	subLoaders = new URLClassLoader[0];
	// No files opened, so should never throw exception
	try{super.close();}catch(IOException _e) {};
}
@Override @Deprecated
public void addURL(URL url){
	throw new UnsupportedOperationException(
		"DynamicClassLoader/addURL is not allowed. Create a new " +
		"classloader instead.");
}

}