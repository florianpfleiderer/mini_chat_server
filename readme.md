# 📡 mini_chat – A Minimal Chat Server in Erlang

**mini_chat** is a simple OTP-style Erlang application that demonstrates how to build a basic chat server using:

- `gen_server` for process behavior  
- `supervision` for fault-tolerance  
- `application` for structured startup  

Each chat user runs in their own Erlang process and can send or receive messages. The chat server also acts as a central dispatcher for general messages.

---

## 📁 Project Structure

```
mini_chat/
├── ebin/                      # Compiled .beam files + .app file
│   ├── mini_chat.app
├── src/                       # Erlang source files
│   ├── mini_chat_app.erl      # Application entry point
│   ├── mini_chat_sup.erl      # Supervisor
│   ├── mini_chat_server.erl   # Chat server process
│   ├── mini_chat_user.erl     # User process module
├── mini_chat.app.src          # Application spec
```

---

## 🛠️ Setup & Compilation

Make sure you have Erlang installed (OTP 21 or later).

### 1. Compile all `.erl` source files

```bash
erlc -o ebin src/*.erl
```

### 2. Start the Erlang shell with code path

```bash
erl -pa ebin
```

---

## 🚀 Running the Application

In the Erlang shell:

```erlang
1> application:load(mini_chat).
2> application:start(mini_chat).
```

You should see:

```erlang
{ok,mini_chat}
```

---

## 💬 Using the Chat System

### Start users

```erlang
3> mini_chat_user:start_link(alice).
4> mini_chat_user:start_link(bob).
```

### Send a message to a user

```erlang
5> mini_chat_user:send(alice, "Hello from Bob!").
```

**Output:**
```
[alice] received: "Hello from Bob!"
```

### Send a message to the server

```erlang
6> mini_chat_server:send_msg(bob, "This is a global server message").
```

**Output:**
```
bob says: "This is a global server message"
```

---

## 🧠 What This App Demonstrates

- How to structure OTP applications
- How to use `gen_server` to encapsulate logic and state
- How to supervise components for reliability
- How to pass messages between processes

---

## 🛡️ Fault Tolerance

If any supervised process crashes, it will be automatically restarted by `mini_chat_sup`, preserving the system’s availability.

---

## 📚 Learn More

- [Erlang OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html)
- [The `gen_server` Behaviour](https://www.erlang.org/doc/man/gen_server.html)
- [Supervisors in OTP](https://www.erlang.org/doc/design_principles/sup_princ.html)

---

## 📦 Future Ideas

- Add a registry for active users
- Allow users to broadcast messages
- Persist chat history in ETS or Mnesia
- Turn into a distributed chat across nodes!

---

