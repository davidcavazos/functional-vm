# Ideas

This is compilation of ideas on how to make this better. 🎉🎉🎉

Reference:

* [The Glasgow Haskell Compiler](http://aosabook.org/en/ghc.html)

## TO DO

* Make typeOf not depend on Package, not return a Result, and move it to its own module
* Change the order of Package input in functions so it's easier to map to collections
* Get rid of `Input` expression, use `Load name typ` instead
* Replace `Function` module with `Expression` module
  * Replace `mapFunctionType` with `withFunctionType`
  * Replace `mapFunction` with `withFunction`
  * Create `withVariables` for `Let`
  * Create `withStatements`
  * Maybe an `asFunction` that gives multiple inputs and statements with a return
* Type aliases
* Builtin functions
* Code generation (start with Python?)
* Elm parser
* Simplify pattern matching tests and validation

## Linter

* Match code base with libraries (standard library only? or only with types invovled in expression, this could involve locally defined functions) to see if there is code duplication.
  * If there is code duplication, warn about how that piece of code could be rewritten using any existing function(s)
* Check for returning or comparing to constant strings, these might be upcasted into types
* Check for similar expressions and help refactor by creating helper functions to reduce duplicated code (check case expressions as well as function calls)
* Check for functions with the same name as other commonly defined functions (like map, andThen, etc) and make sure the types match, otherwise recommend renaming it to something more specific
* Help to automatically refactor code
* Use x :: xs instead of [x] ++ xs
* Use f instead of (\x -> f x)
* Use Tuple.first instead of (\(x, _) -> x)
* Use Tuple.second instead of (\(_, y) -> y)
* Use x -> f x instead of x y -> f x y (common trailing input)
* Use .field instead of (\x -> x.field)
* Simplify (applicable to fold, map, etc)
  * → kvs = [(k,v), (k,v)]
  * → List.fold (\(k, v) x -> f k v x) x0 kvs)
  * → Dict.fold (\k v x -> f k v x) x0 (Dict.fromList kvs)
  * → Dict.fold f x0 (Dict.fromList kvs)
* Use Dict.foldl (\k v -> ...) first Dict instead of List.foldl (\(k, v) -> ...) first (Dict.toList Dict) and similar
* Use Result.map (\_ -> x) instead of Result.andThen (\_ -> Ok x)
* Use x instead of if x then True else False
* Use not x instead of if not x then True else False, or if x then False else True
* Use pipes like f1 |> Result.andThen f2 |> Result.andThen f3 instead of nested functions like Result.andThen (Result.andThen (Result.andThen f3) f2) f1
* Break up large case expressions into their own functions
* Break up large if-then expressions into their own functions
* Mark functions never used
* Mark functions only used once, and maybe move them inside of the function where they’re used (refactor)
* Check if a function may be generalized safely, example List Int → List a
* Split any case expressions with more than 1 (or 3?) indentation level into its own function, even if it’s local
* Split any let definitions with more than 3 indentation levels
* Split any function inputs with more than 3 indentation levels
* In case expressions, order them like
* Constants ints/numbers/tuples/records/constructors -- like [x] -> ..
* Unpacking tuples/records/constructors -- like [x :: xs] -> ..
* Anything/name -- like x -> ..or _ -> ..
* Case expressions with constructors should be defined in the order the type is defined
* Warn (maybe error?) on tuples larger than 3 
* Reorder function inputs to simplify lambda expressions (probably on private functions only, otherwise it could be a breaking change for the public API?)
* Nested pattern matching maybe transform into pattern matching of tuples, make this work as well if there are duplicated cases at different scope levels, but respecting order
* Checking case patterns for returning the same input expression, maybe they can be simplified into a single pattern

## Serialization

* Make the [`dump`](src/Bitcode.elm) functions to use an actual serialization format
  * Human-readable version (default)
    * Easy/fast to parse
  * Bytes version
    * Compress the human-readable version into raw bytes
    * Optimized for fast compression and decompression, then size
    * No need to be cryptographically safe
  * Base64 version
    * Encode to [base64](https://en.wikipedia.org/wiki/Base64) the compressed version

## Assembly

* Compile to a "portable assembly" to generate faster native builds

### Optimizations

* Replace recursion with iteration
* Do lazy evaluation on recursive types
* Recoginze the `map` pattern and parallelize it
* Recognize recursve types like `List` or `Tree`, and use a contiguous array of memory to store them (lists should be compiled into a linked list of arrays)

#### Memory allocation

* Use a fast [memory pool](https://en.wikipedia.org/wiki/Memory_pool) for the memory allocator, some ideas
  * [Buddy memory allocation](https://en.wikipedia.org/wiki/Buddy_memory_allocation)
  * [pymalloc](https://www.evanjones.ca/memoryallocator/)
  * It will have (minimal) internal fragmentation, but it could be very fast and flexible
  * Maybe start with a reasonable "heap" amount like a 2MB memory block (that's what Linux uses for the program's stack memory size, but it should be tunable)
  * Subdivide into a fixed number of memory pools (like 2048 1KB pools, but it should be tunable), the block size would be determined by the allocation patterns of the program
  * Lets say we get a 234 bytes request
    * Determine the closest power of 2 for the block size, in this case 256 bytes
    * If no such pool exists, grab a free memory pool and assign it to handle 256 bytes requests
    * Have a bit vector with all the blocks in that pool
    * Return the block and turn on the first free bit (some fun bit handling :)
    * If we have 1KB pools, that pool could fit only 4 256 byte blocks, if it fills up create a new one
    * If all the pools fill up, create a new memory container
  * Lets say that block is freed
    * Turn off its associated free bit
    * If the pool gets empty, mark it as free again
    * If all pools are marked free and there are more memory containers with at least some free memory (like 20% or 40%, but should be tunable), then delete that container

## Error messaging

* Every time there is a [`fail`](src/Context.elm), push the error into a stack instead of blindly replacing the existing error
* Save the source filename and contents in the context to print code snippets alongside the error messages
  * Save the row and column locations for the start and end of each expression
* Mention common mistakes and how to fix them
* Link to a wiki page for a more in-depth error description

## Debugging

* ~~Maybe have debugging instructions?~~
* Automatically generate a debug version with more metadata
  * The metadata could be stored in a separate file to make it easy to run for production and debug at the same time
