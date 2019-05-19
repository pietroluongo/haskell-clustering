# Haskell Clustering Project


About
-------------

Project from the functional programming course at UFES, _Universidade Federal do Espírito Santo_. This project consists on grouping points together following a previously determined specification. These rules can be found at the _PDF_ file on the root folder of this project. 

The idea behind the project is applying concepts seen in topics such as Linear Algebra and Machine Learning to a different paradigm (the functional one). That way, it is possible to compare directly topics seen in structural programming languages, such as C and C++ with their counterparts in Haskell.

Coding Standard
-------------
**Function comment structure**

    -- Brief function description.
    -- Info about function dependency (e.g. "This should be called by X").
    -- Parameters:
    --    firstParamName: FirstParamType -> First parameter description
    --    secondParamName: SecondParamType -> Second parameter description
    -- Result:
    --    Type -> Result description
    -- Additional notes about the function
**Data type comment structure**
    
    -- Brief datatype description.
    -- Fields:
    	-- firstFieldName: FirstFieldType -> First field description
    
**Parameter Names: `camelCase`**

**Function Names: `camelCase`**

Building Instructions
-------------

There is a Makefile included with the project that will build the project properly. Build possibilities:


- `all`: Standard build procedure, builds all files with optimizations.
- `all-no-optimizations`: Builds the code without optimizations enabled.
- `clean`: Removes compiled binaries and intermediate code.


Folder Structure
-------------

```
.
├── Algebra
│   ├── Group.hs
│   └── Point.hs
├── Main.hs
├── Makefile
├── README.md
└── specification.pdf
```

Some thoughts on the project
-------------

It is quite challenging to shift programming paradigms, as someone who has always used procedural and object-oriented languages, such as C and Java. As such, some of the concepts end up "bleeding" to the code written in what would be a purely functional code. However, i believe that it is a natural approach to try and fit what you already know into a new and different environment, and it is how you end up working with what you're given. Part of programming is being flexible enough to be able to use different tools for different jobs.

I chose to use English as the main language of this project, as i believe it enables a broader group of people to access the source code and the documentation, and it also doubles as a way to train writing in a second language. Furthermore, most programming projects are written in English, as it won't use Unicode characters and improves the homogeneity of the source code as it is easier to read code written in a single language rather than mixing variables and function names in two different languages.

The lack of a debugger in Haskell is something i definitely don't like. Also, the stack trace errors are awful. More often than not they're of absolutely no help at all on solving the simplest errors.

About commits: i prefer to micromanage commits and sometimes it may feel a bit obnoxious to have various commits with small changes. It's an habit i picked up after losing a lot of data, more than once. It wasn't very nice.

I do realize that some of the decisions i took on this project might seem like overkill (like defining a data type Point, when it could be represented as a list of numbers). This was a deliberate decision, simply because i wanted to learn as much as i could from the language, and apply as many different resources as possible to make the project expandable.

Messing with OOP concepts in functional languages is kind of a huge headache. It gives you the tools to do it, but they often feel incomplete and really like an afterthought instead of something that is, in fact, practical to use. Perhaps i'm overthinking it, but i feel like i'm writing more code to make the same procedures i'd make in very few lines on other languages. I believe it's just because i'm not used to it yet, and everything will just "click" eventually. What worries me is the lack of time to finish this project and deal with other projects at the university. I'm kind of mentally exhausted to be honest. 

Well, finally finished this up. Final conclusions are that I/O in Haskell kind of sucks. Formatting the output file was a massive nightmare, but i pushed through thanks to [@chamatt](https://github.com/chamatt) (thanks, dude!). It wound up being easier than i thought initally, but i kind of got stuck on the grouping algorithm for a considerable time. But i guess everything works itself out eventually, if you keep trying. Well, i guess that's about it!
