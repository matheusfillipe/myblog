const DEFAULT_USERNAME = 'matt.tk'
const DEFAULT_NICK = 'mtk-'

export class WsIrcClient {
  constructor(config) {
    let server, port, username, nick, debug, is_ssl
    ({server, port, nick, username, is_ssl, debug} = config)
    if (!server || !port) {
      throw new Error('No server or port specified')
    }
    this.server = server + ":" + port
    this.port = port
    this.nick = nick || DEFAULT_NICK + Math.floor(Math.random() * 10000)
    this.username = username || DEFAULT_USERNAME
    this.is_ssl = is_ssl
    this.debug = debug
    this.log = (c, l) => setTimeout(
      console.log.bind(this, "%c%s", "font-size: 14px; color:" + c, new Date().toLocaleString("it") + ": " + l)
    )
    this.deb = (c, l) => {
      if (this.debug) this.log(c, l)
    }
    this.init = 0
    this.reset_hooks()
  }

  reset_hooks() {
    const empty = () => {}
    const hook_types = ["onmessage", "onjoin", "onconnect", "onclose", "onerror", "onopen", "onnickinuse", "onnames", "onnickchange", "onpart"]
    for (const hook of hook_types) {
      const _hook = "_" + hook
      this[_hook] = empty
      this[hook] = (f) => {
        this[_hook] = f.bind(this)
        return this
      }
    }
  }

  connect() {
    // check if page is ssl
    let wsstring = this.is_ssl ? "wss://" : "ws://"
    let ws = new WebSocket(`${wsstring}${this.server}`)

    ws.onopen = e => {
      ws.send(`user ${this.username} * * :${this.username}`)
      ws.send(`nick ${this.nick}`)
      this._onopen(e)
    }
    ws.onclose = e => {
      this.log("CLOSED")
      this._onclose(e)
    }

    ws.onerror = e => {
      this.log("red", "ERROR")
      fetch("https://" + server).then(c => this.log(c.text()))
      this._onerror(e)
    }

    ws.onmessage = m => {
      if (m.data.indexOf("PING") == 0) ws.send(m.data.replace("PI", "PO"))
      else this.deb("green", "==> " + m.data)

      const irc_code = m.data.split(" ")[1]
      if ((this.init == 0) && (irc_code == "376")) {
        this.init = 1
        this.log("orange", "CONNECTED")
        this._onconnect()
      }

      if (this.init == 1) {
        let type = m.data.split(" ")[1]
        if (type === "PRIVMSG") {
          const channel = m.data.split(" ")[2]
          const from_nick = m.data.split(":")[1].split("!")[0]
          const message = m.data.split(" ").splice(3).join(" ").substring(1)
          this._onmessage(channel, from_nick, message)
          this.deb("blue", from_nick + "> " + message)

        } else if (type === "JOIN") {
          const channel = m.data.split(":")[2].trim()
          this.deb("blue", channel + " joined.")
          this._onjoin(channel)

        } else if (type === "NICK") {
          const new_nick = m.data.split(":")[2]
          this._onnickchange(new_nick)

        } else if (type === "PART") {
          const channel = m.data.split(" ")[2]
          this._onpart(channel)


        } else {
          switch (irc_code) {
            case "433":
              this.deb("red", "Nickname already in use.")
              this._onnickinuse()
              break
            case "353":
              const channel = m.data.split("=")[1].split(" ")[1]
              const names = m.data.split(":").slice(-1)[0].split(" ")
              this._onnames(channel, names)
              break
          }
        }
      } else if (irc_code == "433") {
        this.deb("red", "Nickname already in use. Trying to change nick.")
        this.nick = this.nick + "_"
        this.nick(this.nick)
      }
    }
    this.ws = ws
    return this
  }

  quote(message) {
    message = message.replace(/\n/g, "")
    this.deb("orange", "<== " + message)
    this.ws.send(message)
  }
  send(nick_or_channel, message) {
    this.quote(`PRIVMSG ${nick_or_channel} :${message}`)
  }
  set_nick(new_nick) {
    this.quote(`nick ${new_nick}`)
  }
  join(channel) {
    this.quote(`JOIN ${channel}`)
  }
  part(channel) {
    this.quote(`PART ${channel}`)
  }
  quit(message) {
    this.quote(`QUIT :${message}`)
  }
  mode(channel, mode) {
    this.quote(`MODE ${channel} ${mode}`)
  }
  kick(channel, nick, message) {
    this.quote(`KICK ${channel} ${nick} :${message}`)
  }
  topic(channel, topic) {
    this.quote(`TOPIC ${channel} :${topic}`)
  }
  names(channel) {
    this.quote(`NAMES ${channel}`)
  }
  close() {
    this.quit(window.location.toString())
    this.ws.close()
  }
}

export function getParams(func) {

    // String representaation of the function code
    var str = func.toString();

    // Remove comments of the form /* ... */
    // Removing comments of the form //
    // Remove body of the function { ... }
    // removing '=>' if func is arrow function
    str = str.replace(/\/\*[\s\S]*?\*\//g, '')
            .replace(/\/\/(.)*/g, '')
            .replace(/{[\s\S]*}/, '')
            .replace(/=>/g, '')
            .trim();

    // Start parameter names after first '('
    var start = str.indexOf("(") + 1;

    // End parameter names is just before last ')'
    var end = str.length - 1;

    var result = str.substring(start, end).split(", ");

    var params = [];

    result.forEach(element => {

        // Removing any default value
        element = element.replace(/=[\s\S]*/g, '').trim();

        if(element.length > 0)
            params.push(element);
    });

    return params;
}
