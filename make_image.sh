#!/bin/bash

set -o errtrace
set -o nounset

ROOT="$( cd $( dirname $0 ); pwd )"

BAREBOX_IMG_FILENAME="barebox-phytec-phycore-imx6ul-emmc-512mb.img"
BAREBOX_RELDIR="barebox"
OUTPUT_FILE="$PWD/grisp2_emmc.img.gz"
LOOPBACK_ID="99"

TOOLCHAIN_ROOT=""
BAREBOX_IMG=""
ERLANG_APP_DIR="$ROOT"
ERLANG_APP_NAME=""
ERLANG_APP_VSN=""
FORCE=0
TRUNCATE=1

function usage() {
	local code="${1:-0}"
	echo "USAGE: $0 [-h] [-d] [-f] -t TOOLCHAIN_ROOT -n APP_NAME -v APP_VSN [-i BOOTLOADER_IMG] [-a ERLANG_APP] [-l LOOPBACK_ID] [-o OUTPUT_IMG]"
	echo "  -h: Show this help"
	echo "  -d: Show debugging"
	echo "  -f: Force the overwrite of the output files"
	echo "  -t TOOLCHAIN_ROOT: GRiSP 2 full toolchain directory"
	echo "    e.g. /opt/grisp/grisp2-rtems-toolchain"
	echo "  -n APP_NAME: Name of the erlang release do deploy"
	echo "    e.g. grisp_demo"
	echo "  -v APP_VSN: Version of the erlang release do deploy"
	echo "    e.g. 0.1.0"
	echo "  -i BOOTLOADER_IMG: GRiSP 2 bootloader image"
	echo "    default: TOOLCHAIN_ROOT/$BAREBOX_RELDIR/$BAREBOX_IMG_FILENAME"
	echo "  -a ERLANG_APP: Erlang application to be deployed on partition A"
	echo "    default: $ERLANG_APP_DIR"
	echo "  -l LOOPBACK_ID: Number of the loopback device to use"
	echo "    default: $LOOPBACK_ID"
	echo "  -o OUTPUT_IMG: Output image"
	echo "    default: $OUTPUT_FILE"
	echo "e.g."
	echo "       $0 -t /opt/grisp/grisp2-rtems-toolchain -n grisp_demo -v 1.0.1"
	exit $code
}

function error() {
	local code="$1"
	shift
	local msg="$@"
	echo "ERROR: $msg ($code)"
	usage
}

UNAME="$(uname -s)"
case "${UNAME}" in
	Linux*)
		;;
	*)
		error 1 "Only supported on Linux"
esac

while getopts "hdft:i:a:n:v:l:o:" o; do
	case "${o}" in
		h)
			usage
			;;
		d)
			export DIAGNOSTIC=1
			set -x
			;;
		f)
			FORCE=1
			;;
		t)
			TOOLCHAIN_ROOT="${OPTARG}"
			;;
		i)
			BAREBOX_IMG="${OPTARG}"
			;;
		a)
			ERLANG_APP_DIR="${OPTARG}"
			;;
		n)
			ERLANG_APP_NAME="${OPTARG}"
			;;
		v)
			ERLANG_APP_VSN="${OPTARG}"
			;;
		l)
			LOOPBACK_ID="${OPTARG}"
			;;
		o)
			OUTPUT_FILE="${OPTARG}"
			;;
		*)
			usage 1
			;;
	esac
done
shift $((OPTIND-1))

OUTPUT_FILE="$( cd $( dirname "$OUTPUT_FILE" ); pwd )/$( basename "$OUTPUT_FILE" )"

if [[ "$TOOLCHAIN_ROOT" == "" ]]; then
	error 1 "Missing toolchain parameter (-t)"
fi

if [[ "$BAREBOX_IMG" == "" ]]; then
	BAREBOX_IMG="$( cd "$TOOLCHAIN_ROOT/$BAREBOX_RELDIR"; pwd )/$BAREBOX_IMG_FILENAME"
fi

if [ ! -d "$TOOLCHAIN_ROOT" ]; then
	error 1 "GRiSP 2 toolchain not found at $TOOLCHAIN_ROOT"
fi

if [ ! -f "$BAREBOX_IMG" ]; then
	error 1 "GRiSP 2 bootloader not found at $BAREBOX_IMG"
fi

if [[ "$ERLANG_APP_NAME" == "" ]]; then
	error 1 "Missing Erlang application release name parameter (-n)"
fi

if [[ $ERLANG_APP_VSN == "" ]]; then
	error 1 "Missing Erlang application release version parameter (-v)"
fi

if [ ! -d $(dirname "$OUTPUT_FILE") ]; then
	error 1 "Ouput file directory not found: $OUTPUT_FILE"
fi

if [ ! -d "$ERLANG_APP_DIR" ]; then
	error 1 "Erlang application not found: $ERLANG_APP_DIR"
fi

if [[ $FORCE == 1 ]]; then
	rm -f "$OUTPUT_FILE"
else
	if [ -f "$OUTPUT_FILE" ]; then
		error 1 "Output file already exists: $OUTPUT_FILE"
	fi
fi

LOOPBACK_DEV="/dev/loop$LOOPBACK_ID"
LOOPBACK_PART1="${LOOPBACK_DEV}p1"
LOOPBACK_PART2="${LOOPBACK_DEV}p2"
TEMPDIR=$( mktemp -d -p "$PWD" )
TEMPFILE1=$( mktemp -p "$PWD" )
TEMPFILE2=$( mktemp -p "$PWD" )
trap onexit 1 2 3 15 ERR

function onexit() {
	echo "*** CLEANING UP..."
	if [ -d "$TEMPDIR" ]; then
		sudo umount "$TEMPDIR"
		sudo rmdir "$TEMPDIR"
	fi
	if [ -e "$LOOPBACK_DEV" ]; then
		sudo losetup -d "$LOOPBACK_DEV"
	fi
	if [ -f "$TEMPFILE1" ]; then
		sudo rm -f "$TEMPFILE1"
	fi
	if [ -f "$TEMPFILE2" ]; then
		sudo rm -f "$TEMPFILE2"
	fi
	exit 1
}

echo "**************************************************"
echo "*** BOOTLOADER: $BAREBOX_IMG"
echo "*** ERLANG APP: $ERLANG_APP_DIR"
echo "**************************************************"

echo "*** CREATING BASE IMAGE FILE..."
dd if=/dev/zero of="$TEMPFILE1" bs=1M count=516
echo "*** WRITING BOOTLOADER..."
dd if="$BAREBOX_IMG" of="$TEMPFILE1" conv=notrunc
echo "*** WRITING PARTITION TABLE..."
sudo modprobe loop
sudo losetup "$LOOPBACK_DEV" "$TEMPFILE1"
sudo sfdisk "$LOOPBACK_DEV" << EOF
unit: sectors

$LOOPBACK_PART1 : start=8192, size=524288, Id=83
$LOOPBACK_PART2 : start=532480, size=524288, Id=83
EOF
echo "*** FORMATING SYSTEM PARTITION A..."
sudo partprobe "$LOOPBACK_DEV"
sudo mkfs.vfat -n "GRISP2A" -s 8 "$LOOPBACK_PART1"
echo "*** MOUNTING SYSTEM PARTITION A..."
sudo mount -o uid="$USER" "$LOOPBACK_PART1" "$TEMPDIR"

cd "$ERLANG_APP_DIR"
echo "*** COMPILING ERLANG APPLICATION..."
rebar3 compile
echo "*** DEPLOYING ERLANG APPLICATION TO SYSTEM PARTITION A..."
rebar3 grisp deploy --destination="$TEMPDIR" --pre-script="true" --post-script="true" --relname="$ERLANG_APP_NAME" --relvsn="$ERLANG_APP_VSN"

echo "*** BUILDING RTEMS DEMO..."
cd "$TOOLCHAIN_ROOT"
make demo
echo "*** DEPLOYING RTEMS DEMO TO SYSTEM PARTITION A..."
cp demo/b-imx7/demo.zImage "$TEMPDIR"
DTBFILE="$( cd "$TEMPDIR"; find . -name imx6ul-grisp2.dtb )"
if [ ! -f "${TEMPDIR}/${DTBFILE}" ]; then
	echo "ERROR: Device tree not found in deployed erlang application"
	false # jump to the cleanup hook
fi
cat > "${TEMPDIR}/loader/entries/rtems.conf" <<EOF
title        GRiSP RTEMS Demo
version      0.1.0
linux        /demo.zImage
devicetree   $DTBFILE
architecture ARM
EOF

echo "*** CLEANING UP..."
sudo umount "$TEMPDIR"
sudo rmdir "$TEMPDIR"
sudo losetup -d "$LOOPBACK_DEV"

if [[ $TRUNCATE == 1 ]]; then
	echo "*** TRUNCATING SYSTEM PARTITION B..."
	rm -f "$TEMPFILE2"
	mv "$TEMPFILE1" "$TEMPFILE2"
	dd if="$TEMPFILE2" of="$TEMPFILE1" bs=1M count=260
	rm -f "$TEMPFILE2"
fi

echo "*** COMPRESSING OUTPUT IMAGE..."
gzip -c "$TEMPFILE1" > "$OUTPUT_FILE"

rm -f "$TEMPFILE1"
rm -f "$TEMPFILE2"


echo "**************************************************"
echo "*** DONE: ${OUTPUT_FILE}"
echo "**************************************************"
echo "*** - Copy the image to an SD card:"
echo "***     macOS: $ cp \"${OUTPUT_FILE}\" /Volumes/GRISP"
echo "***            $ diskutil umount /Volumes/GRISP"
echo "***     Linux: $ cp \"${OUTPUT_FILE}\" /media/$USER/GRISP"
echo "***            $ umount /media/$USER/GRISP"
echo "*** - Open a serial console to the GRiSP board"
echo "***     $ screen /dev/tty.usbserial-010031 115200"
echo "*** - Insert the SD card"
echo "*** - Reset the GRiSP board using the onboard reset button"
echo "*** - Enter into barbox console mode by pressing any key before 3 seconds"
echo "*** - Execute command:"
echo "***     $ uncompress /mnt/mmc/$( basename "$OUTPUT_FILE" ) /dev/mmc1"
echo "*** - Remove the SD card"
echo "*** - Reset the GRiSP board again"
echo "**************************************************"
