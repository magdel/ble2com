# ble2com

[![License](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/magdel/ble2com/blob/main/LICENSE.txt)
[![Hits-of-Code](https://hitsofcode.com/github/magdel/ble2com?branch=main&label=Hits-of-Code)](https://hitsofcode.com/github/magdel/ble2com/view?branch=main&label=Hits-of-Code)

Reads BLE data and writes to COM-port so it can be readout as COM-port via virtual null-modem (one like https://sourceforge.net/projects/com0com/)

## Usage

Start application and select device and service to read from.
Also use bledevice.ini to specify COM-port. And you may set device and service to select and connect automatically.

![Sample running view](docs/images/img.png)

### Releases

For direct downloads, check out [Releases](../../releases).

## Contributing

For simple bug reports and fixes, and feature requests, please simply use projects
[Issue Tracker](../../issues)

#### Third-party Dependencies

Pascal Bindings For SimpleBLE Library is Copyright (c) 2022 Erik Lins and released under the MIT License.
https://github.com/eriklins/Pascal-Bindings-For-SimpleBLE-Library

The SimpleBLE library is Copyright (c) 2021-2022 Kevin Dewald and released under the MIT License.
https://github.com/OpenBluetoothToolbox/SimpleBLE
