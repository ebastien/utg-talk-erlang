!SLIDE 
# Erlang #
## ##
<div class="title_desc">
  <ul>
    <li>Emmanuel Bastien</li>
    <li>January 2012</li>
    <li>UtG</li>
  </ul>
</div>

!SLIDE
## Who is talking? ##
* Emmanuel Bastien (@ebastien)
* Development Support for DEV-ORI
* Co-organizer of [Riviera Ruby Group](http://rivierarb.fr)

!SLIDE
## Why learning Erlang? ##
*A language that doesn't affect the way you think about programming, is not worth knowing.*
<div class="title_desc">
  <ul><li>Alan Perlis</li></ul>
</div>

!SLIDE
## History ##
* A proprietary language at Ericsson from 1986 to 1998
* Initialy developped by Joe Armstrong
* Open sourced in 1998
* A programming language to tackle concrete, industrial issues in the
  field of telephony

!SLIDE
## The Erlang language ##
* Functional
* Concurrent
* Simple
* Pragmatic

!SLIDE
## A functional language ##
* Single assignment and referential transparency
* Pattern matching
* Recursion and tail-call optimization
* Higher order functions

!SLIDE
## Single assignment ##
    @@@erlang
    X = 2.
    X = 3. % Error! You told me that X equals 2 ...

!SLIDE
## Pattern matching ##
    @@@erlang
    {{_,ok,X},[_|Y]} = {{0,ok,7.2}, [1,2,3]}.
    X. % 7.2
    Y. % [2,3]

!SLIDE
## Recursion ##
    @@@erlang
    fac(0) -> 1;
    fac(N) when N > 0, is_integer(N) -> N * fac(N-1).

!SLIDE
## Higher order functions ##
    @@@erlang
    lists:foldl(fun(X, Sum) -> X + Sum end, 0, [1,2,3,4]).

!SLIDE
## A simple language ##
* Dynamic typing
* 7 primitive types: Integers, Atoms, Floats, Binaries, Funs, Tuples, Lists
* 3 technical types: References, Pids, Ports
* Libraries for arrays, sets, dictionaries... derived from primitive types

!SLIDE
## Bit syntax ##
    @@@erlang
    <<SourcePort:16, DestinationPort:16,
      AckNumber:32,
      DataOffset:4, _Reserved:4, Flags:8, WindowSize:16,
      CheckSum: 16, UrgentPointer:16,
      Payload/binary>> = SomeBinary.

!SLIDE
## A concurrent language ##
* A process is a, lightweight, first-class citizen.
* Inter-process communication follows the Actor model (i.e share nothing).
* Concurrency =/= Prallelism

!SLIDE
## Runtime ##
* Scales to millions of concurrent processes
* Enforces fault-isolation (a.k.a *let it crash*)
* Supports hot code loading
* An OS inside your OS

!SLIDE
## Erlang sweet spots ##
* Binary protocols
* Massively concurrent servers

!SLIDE
## Références ##
* <a href="http://www.erlang.org/doc">http://www.erlang.org/doc</a>
* <a href="http://learnyousomeerlang.com/">http://learnyousomeerlang.com/</a>
* <a href=""></a>

