# grisp_demo

Demo application for GRiSP boards

## Generate GRiSP EMMC Image

**WARNING** This script has only been tested on Linux **WARNING**

First you need the full toolchain already built:

	$ cd {{GRISP_TOOLCHAIN_ROOT}}
	$ make install

Then you need to build OTP for the project. rebar3 may be able to download a
pre-built image of OTP, if it is not working you should add this to rebar.config
in the `grisp` section:

	{platform, grisp2},
    {build, [
        {toolchain, [
            {directory,"{{GRISP_TOOLCHAIN_ROOT}}/rtems/5"}
		]}
    ]},

replacing GRISP_TOOLCHAIN_ROOT by the path to the toolchain checkout built previously.

Then you can build OTP with:

	$ rebar3 grisp build

Now you can generate the EMMC demo image:

	./make_image.sh -t {{GRISP_TOOLCHAIN_ROOT}} -n grisp_demo -v 0.1.0


## Flashing EMMC Image

To flash the image to the GRiSP board:

1. Copy the image to an SD card:

	**macOS**
	```
	cp grisp2_emmc.img.gz /Volumes/GRISP
	```

	**Linux**
	```
	cp grisp2_emmc.img.gz /media/$USER/GRISP
	```

2. Unmount the SD card:

	**macOS**
	```
	diskutil umount /Volumes/GRISP
	```

	**Linux**
	```
	umount /media/$USER/GRISP
	```

3. Insert the SD card in the GRiSP board.

4. Open a serial console to the board:

	**macOS**
	```
	$ screen /dev/tty.usbserial-010031 115200
	```

4. Reset the board using the button on the board.

5. Open the bootloader console by pressing any key in the serial console
   before 3 seconds.

6. Flash the compressed image, this could take some minutes:

	```
	:/ uncompress /mnt/mmc/grisp2_emmc.img.gz /dev/mmc1
	```

7. Remove the SD card.

8. Reset the GRiSP board again, and it should boot the erlang demo.

9. To boot the RTEMS demo, go to the bootloader console by reseting and 
   pressing a key before 3 seconds, then running:

	```
	:/ boot -m /mnt/emmc
	```

	That would list all the possible boot entries on the EMMC
	select `GRiSP RTEMS Demo` to start the toolchain demo and get an RTEMS
	shell.
