#!/bin/bash

iptables -F
iptables -X
iptables -Z

iptables -P INPUT DROP
iptables -P FORWARD DROP
iptables -P OUTPUT ACCEPT

iptables -A INPUT -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
iptables -A INPUT -i lo -j ACCEPT -m comment --comment "Accept loopback"
iptables -A INPUT -p icmp --icmp-type 3 -j ACCEPT -m comment --comment "Destination Unreachable"
iptables -A INPUT -p icmp --icmp-type 11 -j ACCEPT -m comment --comment "Time Exceeded"
iptables -A INPUT -p icmp --icmp-type 12 -j ACCEPT -m comment --comment "Parameter Problem"
iptables -A INPUT -p udp -m multiport --sports 67,68 -m multiport --dports 67,68 -j ACCEPT -m comment --comment 'DHCP'
iptables -A INPUT -p udp --sport 1900 -j ACCEPT -m comment --comment 'UPnP source'
iptables -A INPUT -p udp --dport 1900 -j ACCEPT -m comment --comment 'UPnP destination'
iptables -R INPUT -p udp -s 192.168.86.0/24 --sport 9999 --dport 9999 -j ACCEPT -m comment --comment 'Google Home'
iptables -A INPUT -p udp -m multiport --dports 137,138 -j ACCEPT -m comment --comment 'samba service sports'
iptables -A INPUT -p tcp -m multiport --dports 139,445 -j ACCEPT -m comment --comment 'samba service sports'
iptables -A INPUT -p tcp --syn --dport 113 -j REJECT --reject-with tcp-reset -m comment --comment "Reject IDENTITY request probing"
iptables -A INPUT -j LOG --log-prefix "iptables: INPUT: DROP: " -m comment --comment "Log dropped packets"
