import socket
import json

def parse_json(filename):
    with open(filename) as data_file:
        data = json.load(data_file)

    if not 'local' in data:
        data['local'] = None

    if not 'cross_op' in data:
        data['cross_op'] = -1

    return data


def send_msg(sock, msg):
        #totalsent = 0
        #while totalsent < MSGLEN:
        #    sent = self.sock.send(msg[totalsent:])
        #    if sent == 0:
        #        raise RuntimeError("socket connection broken")
        #    totalsent = totalsent + sent
        sock.send( msg.encode() )

def open_socket(port):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect(('localhost', port))
    print '[+] New connection at localhost:%d' % port
    return sock
