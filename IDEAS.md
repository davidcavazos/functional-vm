# Ideas

This is compilation of ideas on how to make this better. 🎉🎉🎉

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
