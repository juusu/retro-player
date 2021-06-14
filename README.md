# Retro Player v1.0.0

## What is this?

It's is a playroutine written in M68K assembly for Amiga computers, to play
.RCM music files.

RCM is a new Amiga music format. It's designed to be an universal format.
Theoretically, it should support converting from any music player which
doesn't modify the sample data while the music is playing.

At the moment, the only existing converter supports only ProTracker .MOD
files, you can get it [here](https://github.com/juusu/retro-convert)

I have vague future plans for making converters from AHX, PreTracker and
Future Composer files, time permitting. Any contributions in this regard
are more than welcome, feel free to get in touch for any help should you
wish to contribute with a converter.

## How do I use it?

The source has been written using vasm, but it has also been tested with
AsmOne and should assemble just fine.

You can set some options in your project to change the behaviour of the
player:

* opt_CIA     - set to 1 for CIA timer mode supporting BPM tempo 
                (this is the default setting)
              - set to 0 for VBL mode (need to call the player every
                frame,no BPM tempo support)
* opt_RASTER  - set to 1 if you want the player to show rastertime usage
                (useful for debugging), defaults to 0
* opt_USECODE - can be used to exclude some optional parts of the player
                which can be useful for sizecoding prods to bring the
                size down. default is -1 which includes everything.
                0 includes no optional features and will produce a
                minimal player binary. 

To use, first call rc_Init from your code with the address to the .RCM data in A0.

Then, for CIA mode, call rc_Music to start music playback, and call rc_StopMusic to stop.

For VBlank mode, call rc_Music once every frame. Try to call it near to
the top of the frame, because the raster wait code used is quite crude 
and might hang if the line counter rolls over 255.
