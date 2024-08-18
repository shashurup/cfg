import json
import re
import subprocess
from Xlib.display import Display


def get_string_property(win, property_name):
    return win.get_full_text_property(win.display.get_atom(property_name))


def populate_nodes(root, other_nodes, excluded_node_id=None):
    if root:
        client = root.get('client')
        wid = root.get('id')
        if client and not client.get('floating') and wid != excluded_node_id:
            # other_nodes.append(client['instanceName'])
            other_nodes.append(int(wid))
        else:
            populate_nodes(root.get('firstChild'), other_nodes, excluded_node_id)
            populate_nodes(root.get('secondChild'), other_nodes, excluded_node_id)


SIMPLE_COMMANDS = {
    'close node':               'bspc node --close',
    'kill node':                'bspc node --kill',
    'remove desktop':           'bspc desktop --remove',
    'restart':                  'bspc wm --restart',
    'quit':                     'bspc quit',
    'rotate nodes':             'bspc node --rotate 90',
    'circulate nodes forward':  'bspc node --circulate forward',
    'circulate nodes backward': 'bspc node --circulate backward',
    'balance nodes':            'bspc node --balance',
    'lay node below':           'bspc node --layer below',
    'lay node normal':          'bspc node --layer normal',
    'lay node above':           'bspc node --layer above',
    'set node tiled':           'bspc node --state tiled',
    'set node pseudo tiled':    'bspc node --state pseudo_tiled',
    'set node floating':        'bspc node --state floating',
    'set node fullscreen':      'bspc node --state fullscreen',
    'flag node hidden':         'bspc node --flag hidden',
    'flag node sticky':         'bspc node --flag sticky',
    'flag node private':        'bspc node --flag private',
    'flag node locked':         'bspc node --flag locked',
    'mark node':                'bspc node --flag marked=on',
    'unmark node':              'bspc node --flag marked=off'
}

MONITOR_COMMADS = {
    'focus monitor {}':   'bspc monitor --focus "{}"',
    'move desktop to {}': 'bspc desktop --to-monitor "{}"',
    'move node to {}':    'bpsc node --to-monitor "{}"'
}

DESKTOP_COMMANDS = {
    'focus desktop {}':   'bspc desktop --focus "{}"',
    'swap desktop with {}': 'bspc desktop --swap "{}"',
    'move node to {}':    'bspc node --to-desktop "{}"'
}

NODE_COMMANDS = {
    'focus node {}':     'bspc node --focus "{}"',
    'move node to {}':   'bspc node --to-node "{}"',
    'swap node with {}': 'bspc node --swap "{}"'
}

bspwm = json.loads(subprocess.run('bspc wm --dump-state',
                                  shell=True,
                                  capture_output=True).stdout.decode())

other_monitors = []
other_desktops = []
other_nodes = []
focused_mon_id = bspwm['focusedMonitorId']
for m in bspwm['monitors']:
    is_mon_focused = m['id'] == focused_mon_id
    if not is_mon_focused:
        other_monitors.append(m['name'])
    focused_desk_id = m['focusedDesktopId']
    for d in m['desktops']:
        is_desk_focused = d['id'] == focused_desk_id
        focused_node_id = d.get('focusedNodeId')
        if not (is_mon_focused and is_desk_focused):
            other_desktops.append(d['name'])
            populate_nodes(d['root'], other_nodes)
        else:
            populate_nodes(d['root'], other_nodes, focused_node_id)

max_num = 0
for m in bspwm['monitors']:
    for d in m['desktops']:
        match = re.search(r'(\d+)-.*', d['name'])
        if match:
            num = int(match.group(1))
            if num > max_num:
                max_num = num
            
def add_workspace(name):
    for m in bspwm['monitors']:
        monitor = m['name']
        subprocess.run(f'bspc monitor "{monitor}" --add-desktops "{name}"', shell=True)
        subprocess.run(f'bspc desktop --focus "{name}"', shell=True)
        name += '_'

COMMANDS_WITH_ARGS = [
    ('add desktop', 'bspc monitor --add-desktops "{0}"; bspc desktop --focus "{0}"', f"{max_num+1}-"),
    ('add workspace', add_workspace, f"{max_num+1}-")
]


#print(other_monitors)
#print(other_desktops)
#print(other_nodes)

entries = SIMPLE_COMMANDS

for r in range(1, 10):
    entries['node ratio .{}'.format(r)] = 'bspc node --ratio 0.{}'.format(r)

for k, v in MONITOR_COMMADS.items():
    for mon in other_monitors:
        entries[k.format(mon)] = v.format(mon)

for k, v in DESKTOP_COMMANDS.items():
    for mon in other_desktops:
        entries[k.format(mon)] = v.format(mon)

d = Display()
for wid in other_nodes:
    w = d.create_resource_object('window', wid)
    title = get_string_property(w, '_NET_WM_NAME') or w.get_wm_name()
    for k, v in NODE_COMMANDS.items():
        entry = k.format(title)
        if entry in entries:
            entry += f' ({wid})'
        entries[entry] = v.format(wid)

rofi_input = []
for prefix, _, default_arg in COMMANDS_WITH_ARGS:
    rofi_input.append(prefix + ' ' + default_arg if default_arg else prefix)
rofi_input.extend(entries.keys())
        
result = subprocess.run('rofi -dmenu -p bspwm',
                        shell=True,
                        input='\n'.join(rofi_input).encode(),
                        capture_output=True)
entry = result.stdout.decode().strip()
#print(entry)
cmd = entries.get(entry)
arg = None
if not cmd:
    for prefix, cmd, _ in COMMANDS_WITH_ARGS:
        if entry.startswith(prefix):
            arg = entry[len(prefix):].strip()
            break

#print(cmd)
if isinstance(cmd, str):
    if arg:
        cmd = cmd.format(arg)
    subprocess.run(cmd, shell=True)
else:
    if arg:
        cmd(arg)
    else:
        cmd()
