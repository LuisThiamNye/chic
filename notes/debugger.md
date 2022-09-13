Java Platform Debugger Architecture: https://docs.oracle.com/javase/8/docs/technotes/guides/jpda/index.html

JVM TI is a native interface that can be used to patch existing classes, with limitations.
https://docs.oracle.com/javase/8/docs/platform/jvmti/jvmti.html#RedefineClasses

"The redefinition may change method bodies, the constant pool and attributes. The redefinition must not add, remove or rename fields or methods, change the signatures of methods, change modifiers, or change inheritance. These restrictions may be lifted in future versions."

Existing instances continue running the old methods, but you could choose to pop the frame.

java.lang.instrument package provides instrumentation features of JVM TI
