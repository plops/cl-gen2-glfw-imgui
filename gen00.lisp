(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-cpp-generator2")
  (ql:quickload "cl-ppcre"))

(in-package :cl-cpp-generator2)

(setf *features* (union *features* '(:nolog
				     :debug-thread-activity
				     :serial-debug
				     :queue-debug
				     :lock-debug
				     :adq
				     :finisar
				     )))
(setf *features* (set-difference *features*
				 '(:nolog
				   :debug-thread-activity
				   :serial-debug
				   :queue-debug
				   :lock-debug
				   :adq
				   :finisar
				   )))


(progn
  ;; make sure to run this code twice during the first time, so that
  ;; the functions are defined

  (defparameter *source-dir* #P"../cl-gen2-glfw-imgui/source/")

  (progn
    ;; collect code that will be emitted in utils.h
    (defparameter *utils-code* nil)
    (defun emit-utils (&key code)
      (push code *utils-code*)
      " "))
  (progn
    (defparameter *module-global-parameters* nil)
    (defparameter *module* nil)
    (defun logprint (msg &optional rest)
      `(do0
	" "
	#-nolog
	(do0
	 ;("std::setprecision" 3)
	 (<< "std::cout"
	     "std::endl"
	     ("std::setw" 10)
	     (dot ("std::chrono::high_resolution_clock::now")
		  (time_since_epoch)
		  (count))
					;,(g `_start_time)
	     
	     (string " ")
	     ("std::this_thread::get_id")
	     (string " ")
	     __FILE__
	     (string ":")
	     __LINE__
	     (string " ")
	     __func__
	     (string " ")
	     (string ,msg)
	     (string " ")
	     ,@(loop for e in rest appending
		    `(("std::setw" 8)
					;("std::width" 8)
		      (string ,(format nil " ~a=" (emit-c :code e)))
		      ,e))
	     "std::endl"
	     "std::flush"))))
    (defun guard (code &key (debug t))
		  `(do0
		    #+lock-debug ,(if debug
		       (logprint (format nil "hold guard on ~a" (cl-cpp-generator2::emit-c :code code))
				 `())
		       "// no debug")
		    #+eou ,(if debug
		     `(if (dot ,code ("std::mutex::try_lock"))
			 (do0
			  (dot ,code (unlock)))
			 (do0
			  ,(logprint (format nil "have to wait on ~a" (cl-cpp-generator2::emit-c :code code))
				     `())))
		     "// no debug")
		    "// no debug"
		   ,(format nil
			    "std::lock_guard<std::mutex> guard(~a);"
			    (cl-cpp-generator2::emit-c :code code))))
    (defun lock (code &key (debug t))
      `(do0
	#+lock-debug ,(if debug
	     (logprint (format nil "hold lock on ~a" (cl-cpp-generator2::emit-c :code code))
		       `())
	     "// no debug")

	#+nil (if (dot ,code ("std::mutex::try_lock"))
	    (do0
	     (dot ,code (unlock)))
	    (do0
	     ,(logprint (format nil "have to wait on ~a" (cl-cpp-generator2::emit-c :code code))
			`())))
	
		    ,(format nil
			     "std::unique_lock<std::mutex> lk(~a);"
			     
				(cl-cpp-generator2::emit-c :code code))
		    ))

    
    (defun emit-globals (&key init)
      (let ((l `((_start_time ,(emit-c :code `(typeof (dot ("std::chrono::high_resolution_clock::now")
							   (time_since_epoch)
							   (count)))))
		 ,@(loop for e in *module-global-parameters* collect
			(destructuring-bind (&key name type default)
			    e
			  `(,name ,type))))))
	(if init
	    `(curly
	      ,@(remove-if
		 #'null
		 (loop for e in l collect
		      (destructuring-bind (name type &optional value) e
			(when value
			  `(= ,(format nil ".~a" (elt (cl-ppcre:split "\\[" (format nil "~a" name)) 0)) ,value))))))
	    `(do0
	      (include <chrono>)
	      (defstruct0 State
		  ,@(loop for e in l collect
 			 (destructuring-bind (name type &optional value) e
			   `(,name ,type))))))))
    (defun define-module (args)
      "each module will be written into a c file with module-name. the global-parameters the module will write to will be specified with their type in global-parameters. a file global.h will be written that contains the parameters that were defined in all modules. global parameters that are accessed read-only or have already been specified in another module need not occur in this list (but can). the prototypes of functions that are specified in a module are collected in functions.h. i think i can (ab)use gcc's warnings -Wmissing-declarations to generate this header. i split the code this way to reduce the amount of code that needs to be recompiled during iterative/interactive development. if the module-name contains vulkan, include vulkan headers. if it contains glfw, include glfw headers."
      (destructuring-bind (module-name global-parameters module-code) args
	(let ((header ()))
	  (push `(do0
		  " "
		  (include "utils.h")
		  " "
		  (include "globals.h")
		  " "
		  (include "proto2.h")
		  " ")
		header)
	  (unless (cl-ppcre:scan "main" (string-downcase (format nil "~a" module-name)))
	    (push `(do0 "extern State state;")
		  header))
	  (push `(:name ,module-name :code (do0 ,@(reverse header) ,module-code))
		*module*))
	(loop for par in global-parameters do
	     (destructuring-bind (parameter-name
				  &key (direction 'in)
				  (type 'int)
				  (default nil)) par
	       (push `(:name ,parameter-name :type ,type :default ,default)
		     *module-global-parameters*))))))
  (defun g (arg)
    `(dot state ,arg))
  
  (define-module
      `(main ((_filename :direction 'out :type "char const *")
	      )
	     (do0
	      (include <iostream>
		       <chrono>
		       <cstdio>
		       <cassert>
					;<unordered_map>
		       <string>
		       <fstream>)

	      
	      (let ((state ,(emit-globals :init t)))
		(declare (type "State" state)))


	      (do0
	       (defun mainLoop ()
		 ,(logprint "mainLoop" `())
		 (while (not (glfwWindowShouldClose ,(g `_window)))
		   (glfwPollEvents)
		   (drawFrame)
		   (drawGui)
		   (glfwSwapBuffers ,(g `_window))
		   )
		 ,(logprint "exit mainLoop" `()))
	       (defun run ()
		 ,(logprint "start run" `())
	
		 (initWindow)
		 (initGui)

		 (initDraw)
		 
		 (mainLoop)
		 ,(logprint "finish run" `())))
	      
	      (defun main ()
		(declare (values int))
		
		(setf ,(g `_start_time) (dot ("std::chrono::high_resolution_clock::now")
					     (time_since_epoch)
					     (count)))
		,(logprint "start main" `())
		(setf ,(g `_filename)
		      (string "bla.txt"))

		(do0
		 (run)
		 ,(logprint "start cleanups" `())
		
		 (cleanupDraw)
		 (cleanupGui)
		 (cleanupWindow)
		)
		,(logprint "end main" `())
		(return 0)))))

  
  
  
  (define-module
      `(glfw_window
	((_window :direction 'out :type GLFWwindow* )
	 (_framebufferResized :direction 'out :type bool)
	 )
	(do0
	 
	 (defun keyCallback (window key scancode action mods)
	   (declare (type GLFWwindow* window)
		    (type int key scancode action mods))
	   (when (and (or (== key GLFW_KEY_ESCAPE)
			  (== key GLFW_KEY_Q))
		      (== action GLFW_PRESS))
	     (glfwSetWindowShouldClose window GLFW_TRUE))
	   )
	 (defun errorCallback (err description)
	   (declare (type int err)
		    (type "const char*" description))
	   ,(logprint "error" `(err description)))
	 (defun framebufferResizeCallback (window width height)
	   (declare (values "static void")
		    ;; static because glfw doesnt know how to call a member function with a this pointer
		    (type GLFWwindow* window)
		    (type int width height))
	   ,(logprint "resize" `(width height))
	   (let ((app ("(State*)" (glfwGetWindowUserPointer window))))
	     (setf app->_framebufferResized true)))
	 (defun initWindow ()
	   (declare (values void))
	   (when (glfwInit)
	     (do0
	      
	      (glfwSetErrorCallback errorCallback)
	      
	      (glfwWindowHint GLFW_CONTEXT_VERSION_MAJOR 2)
	      (glfwWindowHint GLFW_CONTEXT_VERSION_MINOR 0)
	      
	      (glfwWindowHint GLFW_RESIZABLE GLFW_TRUE)
	      (setf ,(g `_window) (glfwCreateWindow 930 930
						    (string "vis window")
						    NULL
						    NULL))
	      ,(logprint "initWindow" `(,(g `_window)
					 (glfwGetVersionString)))
	      ;; store this pointer to the instance for use in the callback
	      (glfwSetKeyCallback ,(g `_window) keyCallback)
	      (glfwSetWindowUserPointer ,(g `_window) (ref state))
	      (glfwSetFramebufferSizeCallback ,(g `_window)
					      framebufferResizeCallback)
	      (glfwMakeContextCurrent ,(g `_window))
	      (glfwSwapInterval 1)
	      )))
	 (defun cleanupWindow ()
	   (declare (values void))
	   (glfwDestroyWindow ,(g `_window))
	   (glfwTerminate)
	   ))))
  

  (define-module
      `(draw ((_fontTex :direction 'out :type GLuint))
	     (do0
	      (include <algorithm>)
	      (defun uploadTex (image w h)
		(declare (type "const void*" image)
			 (type int w h))
		(glGenTextures 1 (ref ,(g `_fontTex)))
		(glBindTexture GL_TEXTURE_2D ,(g `_fontTex))
		(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
		(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
		(glTexImage2D GL_TEXTURE_2D 0 GL_RGBA w h 0 GL_RGBA GL_UNSIGNED_BYTE image))
	      
	      (defun initDraw ()
					;(glEnable GL_TEXTURE_2D)
		#+nil (glEnable GL_DEPTH_TEST)
		#+nil (glHint GL_LINE_SMOOTH GL_NICEST)
		#+nil (do0 (glEnable GL_BLEND)
		     (glBlendFunc GL_SRC_ALPHA
				  GL_ONE_MINUS_SRC_ALPHA))
		(glClearColor 0 0 0 1))
	      (defun cleanupDraw ()
		(glDeleteTextures 1 (ref ,(g `_fontTex))))
	      (defun drawFrame ()
		
		(glClear (logior GL_COLOR_BUFFER_BIT
				 GL_DEPTH_BUFFER_BIT)
			 
				 )
		
		))))

  
  (define-module
      `(gui ()
	    (do0
	     "// https://youtu.be/nVaQuNXueFw?t=317"
	     "// https://blog.conan.io/2019/06/26/An-introduction-to-the-Dear-ImGui-library.html"
	     (include "imgui/imgui.h"
		      "imgui/imgui_impl_glfw.h"
		      "imgui/imgui_impl_opengl2.h")
	     (include <algorithm>
		      <string>)
	     (defun initGui ()
	       ,(logprint "initGui" '())
	       (IMGUI_CHECKVERSION)
	       ("ImGui::CreateContext")
	       
	       (ImGui_ImplGlfw_InitForOpenGL ,(g `_window)
					     true)
	       (ImGui_ImplOpenGL2_Init)
	       ("ImGui::StyleColorsDark"))
	     (defun cleanupGui ()
	       (ImGui_ImplOpenGL2_Shutdown)
	       (ImGui_ImplGlfw_Shutdown)
	       ("ImGui::DestroyContext"))
	     
	     (defun drawGui ()
	       #+nil (<< "std::cout"
		   (string "g")
		   "std::flush")
	       
	       (ImGui_ImplOpenGL2_NewFrame)
	       (ImGui_ImplGlfw_NewFrame)
	       ("ImGui::NewFrame")
	       	       
	       (let ((b true))
		      ("ImGui::ShowDemoWindow" &b))
	       ("ImGui::Render")
	       (ImGui_ImplOpenGL2_RenderDrawData
		("ImGui::GetDrawData"))
	       ))))

  
  (progn
    (with-open-file (s (asdf:system-relative-pathname 'cl-cpp-generator2
						      (merge-pathnames #P"proto2.h"
								       *source-dir*))
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (loop for e in (reverse *module*) and i from 0 do
	   (destructuring-bind (&key name code) e
	     (let ((cuda (cl-ppcre:scan "cuda" (string-downcase (format nil "~a" name)))))
	       (unless cuda
		 (emit-c :code code :hook-defun 
			 #'(lambda (str)
			     (format s "~a~%" str))))
	       
	       (write-source (asdf:system-relative-pathname
			      'cl-cpp-generator2
			      (format nil
				      "~a/vis_~2,'0d_~a.~a"
				      *source-dir* i name
				      (if cuda
					  "cu"
					  "cpp")))
			     code)))))
    (write-source (asdf:system-relative-pathname
		   'cl-cpp-generator2
		   (merge-pathnames #P"utils.h"
				    *source-dir*))
		  `(do0
		    "#ifndef UTILS_H"
		    " "
		    "#define UTILS_H"
		    " "
		    (include <vector>
			     <array>
			     <iostream>
			     <iomanip>)
		    
		    " "
		    (do0
		     
		     " "
		     ,@(loop for e in (reverse *utils-code*) collect
			  e)
			
		     
		     " "
		     
		     )
		    " "
		    "#endif"
		    " "))
    (write-source (asdf:system-relative-pathname 'cl-cpp-generator2 (merge-pathnames
								     #P"globals.h"
								     *source-dir*))
		  `(do0
		    "#ifndef GLOBALS_H"
		    " "
		    "#define GLOBALS_H"
		    " "
		    (include <GLFW/glfw3.h>)
		    " "
		    " "
		    (include <thread>
			     <mutex>
			     <queue>
			     <deque>
			     <string>
			     <condition_variable>
			     <complex>)

		    (do0
		     "template <typename T, int MaxLen>"
		     (defclass FixedDequeTM "public std::deque<T>"
		       "// https://stackoverflow.com/questions/56334492/c-create-fixed-size-queue"
		       
		       "public:"
		       (let ((mutex))
			 (declare (type "std::mutex" mutex)))
		       (defun push_back (val)
			 (declare (type "const T&" val))
			 (when (== MaxLen (this->size))
			   (this->pop_front))
			 ("std::deque<T>::push_back" val))))
		    
		    " "
		    ,(emit-globals)
		    " "
		    "#endif"
		    " "))))

