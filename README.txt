 _   _  ___ _____ ___ ____ _____ 
| \ | |/ _ \_   _|_ _/ ___| ____|
|  \| | | | || |  | | |   |  _|  
| |\  | |_| || |  | | |___| |___ 
|_| \_|\___/ |_| |___\____|_____|

The code files in these directories are mostly formated to be used
interactively via the Ocaml toplevel and may not compile as
stand-alone files.  Several examples of error generating code are
present in these files.  Many of these errors are deliberate.  The code files
are intended to be used along with the text of Apress' Practical Ocaml

 _   _  ___ _____ ___ ____ _____ 
| \ | |/ _ \_   _|_ _/ ___| ____|
|  \| | | | || |  | | |   |  _|  
| |\  | |_| || |  | | |___| |___ 
|_| \_|\___/ |_| |___\____|_____|
                                 

All of the code here should compile using the Ocaml 3.09.x.  You may have to change the paths in some of the Makefiles.


    1. Introduction: Why Ocaml

   This chapter will include information about safety and type inference.
   It will also cover the history of the ML family of languages, where
   they fit into the functional programming family tree, and why this
   type of programming is good for solving problems. This chapter will
   also provide the "who this book is for" information.

    2. Interacting with Ocaml: the toplevel

   Here is where we will introduce the various interfaces to Ocaml and
   the few different distributions and where to get them. We will focus
   on the command line (with ledit) and the MS Windows toplevel. We will
   also cover creating a custom toplevel briefly. We create our first
   "hello world" program in Ocaml. We will also discuss the basic files,
   focusing on the code file rather than the interface files for now.

    3. Syntax and Semantics

   We cover types, records, and control flow. We also introduce 'let'
   bindings, variables and comparison operators. We cover these with an
   eye to the fact that Ocaml is a constant language. We spend some time
   talking about the math problem in Ocaml. The math problem is one of
   the more commonly complained about aspects of Ocaml, namely that the
   operators for floats and ints are different. This often causes
   students of the language grief.

    4. Functions: with or without Curry

   Covering both let and let rec function definitions. We also cover
   curried functions: what they are and why the programmer should
   careTP^PT. Given that Ocaml is a constant language, we demonstrate
   accumulators and other recursive methods that do not require
   mutability. We cover pattern matching here, though detailed example
   code is found in Chapters 5 & 10.

   This chapter will also cover anonymous functions.

    5. Practical: A Simple Database

   Using functions, records, and the top-level, we create a simple
   database. This database includes functions for interacting with the
   data and saving it to a file via Ocaml's serialization library.

    6. Primitive and Composite types

   Covering the primitive types found in Ocaml. This includes int, float,
   bool, string, etc. This is more detailed than Chapter 3, and covers
   actually doing stuff with these types. We also cover composite types
   in a more detailed fashion and include some discussion of why types
   matter and how they help the programmer.

    7. Practical: Simple Database Reports and imports

   Using the Simple Database from the earlier example, we create reports
   and imports from strings using Printf and Scanf, as well as the things
   we have learned about primitive types. Using things from the previous
   chapters, we refactor the database records to better reflect the
   problem.

    8. Collections

   Ocaml has a rich set of collections and functions for operating on
   them. We cover iteration, folding, and sequences as well as the
   implications of the collections' feature (when to use what and why).

    9. Files and File I/O

   This chapter is an introduction to channels and their properties. This
   includes sockets. We also look at interacting with the file system and
   pathnames. This chapter includes discussions about the problems file
   I/O presents to functional programming languages.

   10. Exception Handling

   Stack unwinding, exceptions, all the scary stuff that isn't so scary
   since Ocaml is a constant language. We also will cover exception
   handling in classes.

   11. Practical: a URI parsing library

   We implement a URI parsing library that handle HTUfile://UTH uri's
   (using Ocaml's Filename module) and has stubs for handling other uri
   types as well.

   12. Ocamldoc

   This is a short chapter that discusses the ocamldoc. We have given
   examples in Chapter 11, but here we discuss the specifics of Ocamldoc
   and how to make the documentation better.

   13. Modules and Functors

   This chapter covers modules and interfaces. We use the example code in
   Chapter 10 to create a documented module with interface. We also
   introduce functors. We cover how to distribute and install modules, as
   well as findlib basics and creating a findlib META file.

   14. Practical: A Spam Filter

   This is the obligatory naive Bayesian spam filter, with a small twist:
   the module is a functor that takes the scoring function as an
   argument.

   15. Practical: A Network Aware Scoring Function

   Using the client socket support in Ocaml, we will create a network
   based scoring function that allows for querying and updating token
   scores.

   16. Ocamllex and Ocamlyacc

   This is a basic intro, covering differences between Lex and Yacc. This
   will feature an example, but NOT a 4 function calculator. This chapter
   will not cover ASTs.

   17. Practical: Log file scanner using Ocamllex and Ocamlyacc

   Using ocamllex and ocamlyacc, we will create a fast and flexible log
   file scanner using cron logs as an example. Cron log files are spread
   across multiple lines, with other log entries interleaved. We could
   also use a contrived log file that is appropriately complex. 

   18. The Objective part of Ocaml

   In this chapter we discuss classes, their uses and limitations. We
   will cover inheritance too.

   19. Digression: How Ocaml is impure

   Ocaml is not a pure functional programming language. We now cover
   mutability, references, and using classes to hide this kind of thing.

   20. Digression: Functional Programming as a lifestyle choice

   Here we talk about FP and what its ramifications to the world of
   programming. There are many people who say wide, breathless things
   about FP and there are many who say nasty things too. What is a
   programmer to do? And how can a programmer get a chance to use these
   less popular languages?

   This digression is somewhat evangelical.

   21. Practical: Web programming with mod_caml

   Now we're back in the saddle. mod_caml is everyone's favorite
   ocaml-apache module. We do some basic web programming.

   22. Practical: A Shoutcast Server

   We write a shoutcast server. This also creates a generic server
   framework that the user could implement their own arbitrary servers
   from. We also talk about the high-level socket functions.

   23. Threading and Concurrency

   Ocaml supports threads natively, but they are not "real" threads. This
   chapter tries to help the reader understand what this means. We also
   discuss multi-process concurrency and RPC.

   24. Practical: A Concurrent Web Crawler

   We implement a concurrent web crawler that uses threads to crawl many
   sites simultaneously. Using Ocamllex and Ocamlyacc we use the crawler
   to find sites that contain a specific set of words.

   25. FFI: Camlidl and interfacing with C

   We give a brief tutorial on Camlidl, Swig, and wrapper writing in C
   for Ocaml directly. We also talk about why FFI matters in a language
   like Ocaml.

   26. Practical: strftime and difftime

   These are two things missing from the standard Ocaml UNIX library. We
   will create strftime and difftime functions using FFI (camlidl and
   direct C, since the Swig stuff is better documented elsewhere).

   27. Practical: Parsing Binary Files

   We've done a lot of complex text parsing, what about binary data?
   Search for strings; create a program that finds
   the longest identical sections of two binary files.

   28. Makefiles, findlib, and Ocaml Development

   We talk about OcamlMakefile, findlib, and other tools (including
   IDEs). Here we talk about profiling and debugging too. This section
   will probably also mention F# and Visual Studio.

   29. Camlp4

   This is an introduction to Camlp4 and tutorial. Caml4 is a big, and
   rather scary tool since it allows the user to rebuild the Ocaml
   language on the fly. It is similar to the Lisp macro, but not as
   widely used. We will create some macro like actions and a pretty
   printer for the simple database records seen in Chapter 5.

   30. Conclusion
