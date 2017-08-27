# dhcp-discover

If you've chosen static addressing for your DHCP enabled network,
but setting the gateway as DNS doesn't work, this program
can be used to obtain DNS addresses (and update `/etc/resolv.conf` if asked).

## Usage
Run as root:

    # dns-discover
    1.2.3.4
    1.2.3.5
    # dns-discover -u  # fetch addresses and update /etc/resolv.conf
    #

## Build
    $ stack setup # first time only
    $ stack build

