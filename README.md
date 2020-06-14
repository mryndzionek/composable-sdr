# composable-sdr

DSP processing blocks aimed at SDR, embedded in Haskell.

![logo](soapy-sdr.png)

![build](https://github.com/mryndzionek/composable-sdr/workflows/build/badge.svg)

## Introduction

This repo is aimed at exploring the usefulness of data flow programming for
SDR/DSP processing. It leverages [SoapySDR](https://github.com/pothosware/SoapySDR)
for data sources and [liquid-dsp](https://github.com/jgaeddert/liquid-dsp) for radio
DSP. All those low-level C/C++ libraries are fine for 'programming in the small', but when
'programming in the large' code gets ugly really fast. And here is where Haskell comes in.
The idea is to use C/C++ interop to 'lift' low-level APIs to streams and folds in [Streamly](https://hackage.haskell.org/package/streamly).
The hope is to create a framework in which efficient DSP is possible and without sacrificing
code quality even for complex signal processing flows. In other words this will be a 'vehicle'
to connect [liquid-dsp](https://github.com/jgaeddert/liquid-dsp) functions into more complex and powerful topologies
while preserving desired non-functional requirements.

This repo will stay fairly low-level. The applications built using provided processing blocks
should be lean and mean, so that it's possible to deploy them on even not-so-powerful, embedded,
headless systems. All the UI interaction stuff can be always done via sockets and this
is probably one of the best designs for such things.

As of today there is only one fairly minimalistic application implemented, but I
think the approach already shows its benefits. Writing an application with just one set of
functionalities offered by `soapy-sdr`, in an imperative language, would be an accomplishment in itself.
More details below.

## Dependencies

 - [SoapySDR](https://github.com/pothosware/SoapySDR)
 - SoapySDR module(s) like [SoapyRTLSDR](https://github.com/pothosware/SoapyRTLSDR)
 - [liquid-dsp](https://github.com/jgaeddert/liquid-dsp)

Cabal v2 project requires ghc-8.6.5, so before first `cabal v2-build` something like `cabal v2-configure -w /opt/ghc/8.6.5/bin/ghc-8.6.5`
is needed. There is also a `stack.yaml` file available, but might not always be up-to-date, as I'm mostly using
the Cabal v2 setup.

## Libraries and modules

### ComposableSDR

All the C-interop and Streamly streams and folds - one file for now.

## Tools and applications

### soapy-sdr

I/Q recorder and processor using SoapySDR as backend.

#### :warning: All the below screenshots are from other applications. This is purely CLI application.

![soapy-sdr](images/soapy-sdr.png)

Blue arrows are choices in signal flow. First choice is the data source:
SoapySDR compatible SDR receiver, or a CF32/WAV file.
Next choice is usage of the PFB channelizer.
If it's enabled, then the output signals can be written
to separate files, or mixed together and written into one file.
The raw/modulated signal output file format is CF32.
Files can be opened in [inspectrum](https://github.com/miek/inspectrum).

```
Usage: soapy-sdr [--filename NAME [--chunksize INT] | [--devname NAME] 
                   [-f|--frequency DOUBLE] [-g|--gain DOUBLE]] 
                 [-s|--samplerate DOUBLE] [--offset DOUBLE] 
                 [-b|--bandwidth DOUBLE] [-n|--numsamples INT] 
                 [-o|--output FILENAME] [--demod ARG] [-a|--agc DOUBLE] 
                 [-c|--channels INT] [-m|--mix]
  Process samples from an SDR retrieved via SoapySDR

Available options:
  --filename NAME          Input (CF32) file name
  --chunksize INT          Chunk size ins CF32 sample (default: 1024)
  --devname NAME           Soapy device/driver name (default: "rtlsdr")
  -f,--frequency DOUBLE    Rx frequency in Hz (default: 1.0e8)
  -g,--gain DOUBLE         SDR gain level (0 = auto) (default: 0.0)
  -s,--samplerate DOUBLE   Sample rate in Hz (default: 2560000.0)
  --offset DOUBLE          Offset frequency in Hz (default: 0.0)
  -b,--bandwidth DOUBLE    Desired output bandwidth in [Hz] (0 = samplerate = no
                           resampling/decimation) (default: 0.0)
  -n,--numsamples INT      Number of samples to capture (default: 1024)
  -o,--output FILENAME     Output file(s) name (without
                           extension) (default: "output")
  --demod ARG              Demodulation type (default: DeNo)
  -a,--agc DOUBLE          Enable AGC with squelch threshold in [dB] (0 = no
                           AGC) (default: 0.0)
  -c,--channels INT        Number of channels to split the signal
                           into (default: 1)
  -m,--mix                 Instead of outputting separate file for each channel,
                           mix them into one
  -h,--help                Show this help text

```

There is an experimental [AppImage](https://appimage.org/) deployment workflow producing
self-contained binaries from every commit pushed to `build` branch. SoapyRTLSDR module
is bundled inside the image. Other modules should be detectable from the host system
as long they reside in standard path (`/usr/local/lib/SoapySDR/modulesx.y`, etc.) and
are compiled with compatible versions. This however wasn't tested much and long-term
the safest solution might be bundling all the (tested/supported) SoapySDR modules.

Some captures from ISM 433MHz

![spectrum1](images/inspectrum1.png)
![spectrum2](images/inspectrum2.png)

LoRa on 868MHz

![spectrum3](images/inspectrum3.png)

#### Example 1

Let's first check using [CubicSDR](https://cubicsdr.com/) if there are any signals within FM radio band (88-108MHz).

![ex1_1](images/ex1_1.png)

We see a station on 92MHz. Bandwidth of the signal seems to be around 200kHz. Lets record some IQ samples
(2 million sample = 10s of recording, DeNo means no demodulation - output will be CF32 IQ sample file):

```sh
cabal v2-run -- soapy-sdr -n 2000000 -f 92.0e6 -b 200000 --demod "DeNo"
```

Lets now inspect the output file (output.cf32) in [inspectrum](https://github.com/miek/inspectrum).

![ex1_2](images/ex1_2.png)

Now let's record a wideband WAV file with FM demodulated signal:

```sh
cabal v2-run -- soapy-sdr -n 2000000 -f 92.0e6 -b 200000 --demod "DeNBFM 0.6 WAV"
```

Sample rate of this file is 200kHz. Didn't know libsndfile can pull this off :smiley:. On a spectrogram in
[Audacity](https://www.audacityteam.org/) we can clearly see the mono audio below 15kHz, 19kHz stereo pilot, stereo audio
between 23kHz and 53kHz and RDBS around 57kHz.

![ex1_3](images/ex1_3.png)

The same in baudline:

![ex1_4](images/ex1_4.png)

Alright, let's now do proper wide band FM (mono) demodulation with de-emphasis, resampled rate of 192k and output decimation of 4,
to get 48kHz output WAV file:

```sh
cabal v2-run -- soapy-sdr -n 2000000 -f 92.0e6 -b 192000 --demod "DeWBFM 4 WAV"
```

There is also experimental stereo FM decoder:

```sh
cabal v2-run -- soapy-sdr -n 2000000 -f 92.0e6 -b 192000 --demod "DeFMS 4 WAV"
```

It's possible to 'play live' running below commands in a separate terminal:

```sh
rm output*; mkfifo output.au && play output.au
```
and then starting `soapy-sdr`, but with AU audio format set.

#### Example 2

To run as a [PMR446](https://en.wikipedia.org/wiki/PMR446) scanner:

```sh
cabal v2-run -- soapy-sdr -n 2000000 -f 446.1e6 -b 200000 -c 16 -s 1.0e6 \
--demod "DeNBFM 0.3 WAV" -g 40 -a -16
```

This will output 16 WAV files, each for one PMR channel. To merge all the files into one `-m` flag can be used:
There is also AGC with squelch (`-a` option), but needs more testing and adding auto mode.

#### Example 3

Just a general demonstration of precision and efficiency. We're switching the SDR to 3.2MSPS.
Then resample the signal to 1.6MSPS and channelize to 20 channels, writing to 20 separate files.
We request 16M sample (after resampling), so around 10s of recording:

```sh
time cabal v2-run -- soapy-sdr -n 16000000 -f 433.9e6 -s 3.2e6 -b 1.6e6 \
--demod "DeNo" -g 35 -a -50 -c 20
```

Below is a GIF showing how the files are written and CPU utilization. No samples are lost and each file
ends up 6400000 bytes long. One CF32 sample is 8 bytes, so at 3.2MSPS we're capturing around 24MB/s.
Then processing it and saving around 122MB in 10 seconds.

![ex1_5](images/ex1_5.gif)

#### Example 4

Capturing raw CF32 IQ samples file and demodulating offline.

Capturing a slice of FM band at maximum samplerate (3.2MSPS) of RTL-SDR to a file `input.cf32`.
192MS is about a minute of recording and 24MB/s, so overall 1.43GB of data:

```sh
cabal v2-run -- soapy-sdr -n 192000000 -f 91.0e6 -s 3.2e6 --demod "DeNo" -o input
```

I've tuned to 91MHz and we now that at 92MHz there a station. Lets mix down and resample
192kHz wide slice of this part of the spectrum:

```sh
cabal v2-run -- soapy-sdr --filename input.cf32 -n 192000000 -s 3200000 \
--offset 1.0e6 -b 192000 --demod "DeNo"
```

Then we can demodulate the extracted slice of spectrum:

```sh
cabal v2-run -- soapy-sdr --filename output.cf32 -n 192000000 -s 192000 --demod "DeWBFM 4 WAV"
```

#### Example 5

Demodulating signal from a sensor transmitting on 433MHz ISM band (software AGC with squelch enabled):

```sh
cabal v2-run -- soapy-sdr -n 200000 -f 434.388e6 -b 20000 -s 1.0e6 \
--demod "DeNBFM 0.3 WAV" -g 30 -a -50
```

![spectrum4](images/inspectrum4.png)

#### Example 6

Decoding FSK data from a helicopter, similarly to [this](http://www.windytan.com/2014/02/mystery-signal-from-helicopter.html) and [this](https://github.com/proto17/HelicopterDemod/wiki).
First lets download the audio containing FSK signal from [youtube](https://www.youtube.com/watch?v=2MprHxarmOI) using [youtube-dl](https://youtube-dl.org/):

```sh
youtube-dl -x --audio-format wav https://www.youtube.com/watch?v=2MprHxarmOI
```

The file has two channels, so convert it to mono using [Audacity](https://www.audacityteam.org/).
Open the WAV in Auadacity and then `Tracks -> Mix -> 'Mix Stereo down to Mono'`. Finally save the converted file and rename it to `helicopter.wav`.
Convert the WAV file to IQ (.cf32) data file for analysis using [inspectrum](https://github.com/miek/inspectrum):

```sh
cabal v2-run -- soapy-sdr --filename helicopter.wav -n 19200000000 -s 24000 --demod "DeNo"
```

In inspectrum we see the signal between 1.2kHz and 2.4kHz:

![ex6_1](images/ex6_1.png)

Lets now try filter out and FM demodulate the signal:

```sh
cabal v2-run -- soapy-sdr --filename helicopter.wav -n 20000000000 -s 24000 \
--offset 1.8e3 -b 6.0e3 --demod "DeNBFM 0.6 WAV"
```

In the resulting WAV file (`output.wav`) we should see something resembling a square wave:

![ex6_2](images/ex6_2.png)

Not perfect, but we can try to FM demodulate with with timing recovery to get something workable:

```sh
cabal v2-run -- soapy-sdr --filename helicopter.wav -n 20000000000 -s 24000 \
--offset 1.8e3 -b 4.8e3 --demod "DeNBFMSync 4"
```

The `4` signifies 4 samples per symbol (The bitrate is 1200bit/s, we resampled to 4800Hz, so 4800/1200=4).
This will result in `output.f32` file with the demodulated signal and symbols synchronized (2 samples per symbol).

Without symbol synchronizer:

![ex6_5](images/ex6_5.png)

With symbol synchronizer:

![ex6_6](images/ex6_6.png)


Further processing requires some trial and error.
More info can be found [here](https://github.com/proto17/HelicopterDemod/wiki).
There is a simple decoding app in this [repo](apps/HeliDecode.hs) that works slightly differently.
When run like this:

```sh
cabal v2-run -- helidecode output.f32
```

the app will output a KML (`output.kml`) file that can be visualized using [GpsPrune](https://activityworkshop.net/software/gpsprune/):

![ex6_3](images/ex6_3.png)

![ex6_4](images/ex6_4.png)


## TODO
  - [ ] eliminate the need for mandatory `-n,--numsamples` argument
  - [ ] add live playback via PulseAudio
  - [ ] add RF protocol decoders
  - [ ] profile flows and introduce concurrency modifiers (`aheadly`, etc.)
  - [ ] Template Haskell boilerplate code generator for Liquid-DSP blocks
  - [ ] add automatic tests (IQ .cf32 files can be read already, so having a set of them for testing different processing configurations would be a good idea)
