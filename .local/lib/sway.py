from itertools import groupby
import json
import socket
import subprocess
import sys

i3_magic = 'i3-ipc'.encode()

RUN_COMMAND = 0
GET_WORKSPACES = 1
SUBSCRIBE = 2
GET_OUTPUTS = 3
GET_TREE = 4
GET_MARKS = 5
GET_BAR_CONFIG = 6
GET_VERSION = 7
GET_BINDING_MODES = 8
GET_CONFIG = 9
SEND_TICK = 10
SYNC = 11
GET_BINDING_STATE = 12

def create_message(msg_type, payload=''):
    payload_bytes = payload.encode()
    return i3_magic + len(payload_bytes).to_bytes(4, 'little') + msg_type.to_bytes(4, 'little') + payload_bytes

def interact(sock, msg):
    sock.sendall(msg)
    header = sock.recv(14)
    payload_len = int.from_bytes(header[6:10], 'little')
    return json.loads(sock.recv(payload_len))

def list_workspaces(sock):
    return interact(sock, create_message(GET_WORKSPACES))

def list_outputs(sock):
    return interact(sock, create_message(GET_OUTPUTS))

def global_tree(sock):
    return interact(sock, create_message(GET_TREE))

def run_command(sock, cmd):
    return interact(sock, create_message(RUN_COMMAND, cmd))

def workspace_num(workspace_name):
    if workspace_name and workspace_name[0].isdigit():
        return int(workspace_name[0])

def new_workspace_num(workspaces):
    num_set = set(workspace_num(w['name']) for w in workspaces)
    for num in [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]:
        if num not in num_set:
            return num

def run_rofi(options):
    result = subprocess.run('rofi -dmenu -p sway',
                            shell=True,
                            text=True,
                            check=True,
                            capture_output=True,
                            input='\n'.join(options))
    return result.stdout.strip()

def search_node(tree, cond, parents=[]):
    if cond(tree):
        return tree, parents
    for node in tree.get('nodes', []) + tree.get('floating_nodes', []):
        result = search_node(node, cond, [tree] + parents)
        if result:
            return result

def find_parent(parents):
    for parent in parents:
        if len(parent['nodes']) > 1 and parent['layout'] != 'tabbed':
            return parent
        if parent['type'] == 'workspace':
            return parent
        
def find_next_floating_size(win_size, monitor_size):
    win_width, win_height = win_size
    mon_width, mon_height = monitor_size
    tall = win_height > mon_height * 0.8
    wide = win_width > mon_width * 0.6
    if tall and wide:
        return mon_width // 2, mon_height * 95 // 100             # tall window
    if tall:
        mon_width = mon_width * 2 // 3
        return mon_width, mon_width  * 9 // 16                    # 16:9 window
    if wide:
        return mon_width * 95 // 100, mon_height * 95 // 100      # almost fullscreen
    return mon_width // 2, mon_height * 95 // 100                 # tall window 


result = subprocess.run(['sway', '--get-socketpath'], capture_output=True, check=True, text=True)
sock_path = result.stdout.strip()

sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
sock.connect(sock_path)

action = sys.argv[1]
if action == 'workspace':
    prefix = sys.argv[2] if len(sys.argv) > 2 else ''
    wss = list_workspaces(sock)
    target = []
    if prefix:
        target = [ws for ws in wss if ws['name'].startswith(prefix)]
    if target:
        invisible = [ws for ws in target if not ws['visible']]
        if invisible:
            for _, v in groupby(invisible, lambda x: x["output"]):
                ws_name = next(v)["name"]
                run_command(sock, f'workspace {ws_name}')
        else:
            focused = [ws for ws in wss if ws['focused']]
            if focused:
                run_command(sock, f'workspace {focused[0]["name"]}')
    else:
        if not workspace_num(prefix):
            new_num = new_workspace_num(wss)
            if new_num and prefix:
                prefix = f'{new_num}:{prefix}'
            elif not prefix:
                prefix = str(new_num)
        run_command(sock, f'workspace {prefix}')
elif action == 'menu':
    wss = list_workspaces(sock)
    new_num = new_workspace_num(wss)
    options = [f'workspace {new_num}',
               'rename workspace to ',
               'move workspace to output left',
               'move workspace to output right']
    outputs = [o['name'] for o in list_outputs(sock) if o['active']]
    for o in outputs:
        options.append(f'move workspace to output {o}')
    for ws in wss:
        options.append(f'move container to workspace {ws["name"]}')
        options.append(f'move container to workspace {ws["name"]}; workspace {ws["name"]}')
    options.extend(['split horizontal',
                    'split vertical',
                    'layout stacking',
                    'layout tabbed',
                    'layout split',
                    'layout splitv',
                    'layout splith',
                    'floating enable',
                    'floaging disable',
                    'fullscreen enable',
                    'fullscreen disable',
                    'sticky enable',
                    'sticky disable',
                    'border normal',
                    'border pixel',
                    'move scratchpad',
                    'scratchpad show'])
    for g1 in ['current', 'all']:
        for g2 in ['set ', 'plus ', 'minus ', 'toggle']:
            options.append(f'gaps inner {g1} {g2}')
            for g3 in ['outer', 'horizontal', 'vertical', 'top', 'right', 'bottom', 'left']:
                options.append(f'gaps {g3} {g1} {g2}')
    options.extend(['reload',
                    'restart',
                    'exit',
                    'debug log on',
                    'debug log off'])
    i3_cmd = run_rofi(options)
    run_command(sock, i3_cmd)
elif action == 'cycle-size':
    from pprint import pprint as pp
    tree = global_tree(sock)
    node, parents = search_node(tree, lambda x: x['focused'])
    for p in parents: print(p['name'])
    if node and node['fullscreen_mode'] == 0:
        parent = find_parent(parents)
        if node['type'] == 'floating_con':
            width = node['rect']['width']
            height = node['rect']['height']
            ws_width = parent['rect']['width']
            ws_height = parent['rect']['height']
            new_width, new_height = find_next_floating_size((width, height),
                                                            (ws_width, ws_height))
            run_command(sock,
                        f'resize set width {new_width}px height {new_height}px; '
                        'move position center')
        elif parent:
            prop = None
            if parent['orientation'] == 'horizontal':
                prop = 'width'
            elif parent['orientation'] == 'vertical':
                prop = 'height'
            if prop:
                size = node['rect'][prop]
                parent_size = parent['rect'][prop]
                if size < parent_size // 3:
                    new_size = parent_size // 3
                elif size < parent_size // 2:
                    new_size = parent_size // 2
                elif size < parent_size * 2 // 3:
                    new_size = parent_size * 2 // 3
                else:
                    new_size = parent_size // 3
                run_command(sock, f'resize set {prop} {new_size}px')
elif action == 'test':
    from pprint import pprint as pp
    pp(new_workspace_num(list_workspaces(sock)))
