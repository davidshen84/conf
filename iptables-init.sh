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
iptables -A INPUT -p udp -m multiport --dports 137,138 -j ACCEPT -m comment --comment 'samba service ports'
iptables -A INPUT -p tcp --syn --dport 113 -j REJECT --reject-with tcp-reset -m comment --comment "Reject IDENTITY request probing"
iptables -A INPUT -j LOG --log-prefix "iptables: INPUT: DROP: " -m comment --comment "Log dropped packets"
