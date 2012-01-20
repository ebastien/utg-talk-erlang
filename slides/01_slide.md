!SLIDE 
# Erlang #
## Quick overview ##
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
  ([@rivierarb](http://twitter.com/rivierarb))

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
* A language to tackle concrete, industrial issues in the
  field of telephony

!SLIDE
## Erlang today ##
* Riak
* CouchDB / Membase
* RabbitMQ
* ejabberd

!SLIDE
## The Erlang language ##
* Functional
* Simple
* Concurrent
* Distributed
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
    Y = lists:foldl(fun(X, Sum) -> X + Sum end, 0, [1,2,3]).
    Y. % 6

!SLIDE
## A simple language ##
* Dynamic typing
* 7 primitive types: Integers, Atoms, Floats, Binaries, Funs, Tuples, Lists
* 3 technical types: References, Pids, Ports
* Libraries for arrays, sets, dictionaries... derived from primitive types

!SLIDE
## Persistent data structures ##
* No mutable state
* Easier to reason about
* Share without locking

!SLIDE
## Bit syntax ##
    @@@erlang
    <<SourcePort:16, DestinationPort:16,
      AckNumber:32,
      DataOffset:4, _Reserved:4, Flags:8, WindowSize:16,
      CheckSum: 16, UrgentPointer:16,
      Payload/binary>> = SomeBinary.

!SLIDE
## Record syntax ##
    @@@erlang
    -record(flight_period, {flight_key, legs, segments}).
    
    Period = #flight_period{
      flight_key = K,
      legs = [L1, L2],
      segments = [S1, S2, S3]
    }.
    
    Legs = Period#flight_period.legs.
* Syntactic sugar on top of primitive types
* Converted to tuples at compilation time

!SLIDE
## A concurrent language ##
* A process is a, lightweight, first-class citizen.
* Inter-process communication follows the Actor model (i.e share nothing).
* Concurrency =/= Parallelism

!SLIDE
## Inter-process messaging ##
    @@@erlang
    loop() ->
      receive
        hello -> io:format("Unleash the geeks!~n");
        _ -> loop()
      end
    end.
    
    P = spawn(fun loop/0).
    % <0.55.0>
    P ! hello.
    % Unleash the geeks!

!SLIDE
## A distributed language ##
* A distributed runtime over a cluster of Erlang nodes.
* Message passing between remote processes works the same.
* Nodes can be added and removed at anytime.

!SLIDE
## A distributed language ##
    @@@erlang
    myhost$ erl -sname local
    > node().
    % local@myhost
    ...
    myhost$ erl -sname remote
    > node().
    % remote@myhost

!SLIDE
## A distributed language ##
    @@@erlang
    local> F = fun () ->
                 receive
                   whoareyou -> io:format("I am ~s!~n",
                                          [node()])
                 end
               end.
    local> spawn(F) ! whoareyou.
    % I am local@myhost!
    local> spawn('remote@myhost', F) ! whoareyou.
    % I am remote@myhost!

!SLIDE
## A pragmatic language ##
* Not as pure as other functional languages
* IO side effects
* The *process dictionary* is mutable

!SLIDE
## Runtime ##
* Scales to millions of concurrent processes
* Enforces fault-isolation (a.k.a *let it crash*)
* Supports hot code loading
* An OS inside your OS

!SLIDE
## Native Implemented Function ##
    @@@C
    #include <erl_nif.h>
    static ERL_NIF_TERM
    orthodromic_distance(ErlNifEnv *env, int argc,
                         const ERL_NIF_TERM argv[])
    {
      double LonA, LatA, LonB, LatB;
      if (!enif_get_double(env, argv[0], &LonA))
        return enif_make_badarg(env);
      // ...
      double Distance = 2*EARTH_RADIUS*asin(
        sqrt(SinLat*SinLat +
             cos(LatA)*cos(LatB)*SinLon*SinLon));
      return enif_make_double(env, Distance);
    }

!SLIDE
## Multicore support ##
* SMP support implemented in 2006
* Separate heaps vs. shared heap
* Zero-copy messaging not here yet
* Scheduler not tuned for High-Performance Computing

!SLIDE
## Erlang/OTP ##
* Open Telecom Platform
* High-level framework on top of Erlang processes
* Processes coordination and monitoring
* Reusable subsystem designs

!SLIDE
## Erlang sweet spots ##
* Network (binary) protocols
* Massively concurrent servers
* Highly resilient applications

!SLIDE
## References ##
* <a href="http://www.erlang.org/doc">http://www.erlang.org/doc</a>
* <a href="http://learnyousomeerlang.com/">http://learnyousomeerlang.com/</a>
* <a href=""></a>

