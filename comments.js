// Import the functions you need from the SDKs you need
import { setCookie, getCookie, isOnPost } from '/utils.js'
import { initializeApp } from "https://www.gstatic.com/firebasejs/9.6.11/firebase-app.js";
import { getDatabase, ref, set, get, child } from "https://www.gstatic.com/firebasejs/9.6.11/firebase-database.js";

const setupComments = () => {
  if (!isOnPost) {
    return
  }

  // Fill in username if ever commented
  const username = getCookie('username')
  if (username) {
    document.getElementById("comment-name").value = username
    const uname = document.getElementById("comment-name");
    uname.disabled = true
  }

  const firebaseConfig = {
    apiKey: "AIzaSyCBmdPqXaaCU_P0emUZkoM1QGxrFd58EOQ",
    authDomain: "blog-comments-441d2.firebaseapp.com",
    databaseURL: "https://blog-comments-441d2-default-rtdb.firebaseio.com",
    projectId: "blog-comments-441d2",
    storageBucket: "blog-comments-441d2.appspot.com",
    messagingSenderId: "331769743474",
    appId: "1:331769743474:web:c7b6daa9b31ac621181aeb",
    measurementId: "G-65HBJ518LY"
  };

  // Initialize Firebase
  const app = initializeApp(firebaseConfig);

  // Get a reference to the database service
  const database = getDatabase(app);

  // Simple Hash implementation
  String.prototype.hash = function() {
    var hash = 0;
    for (var i = 0; i < this.length; i++) {
      var code = this.charCodeAt(i);
      hash = ((hash << 5) - hash) + code;
      hash = hash & hash; // Convert to 32bit integer
    }
    return Math.abs(hash).toString();
  }

  // Page path
  const path = window.location.pathname.toString()
  const fullpath = path.hash() + "-" + path.split("/").slice(-1).join().replace(".html", "")


  // DB functions
  async function postComment(name, text) {
    const time = Date.now()
    const getpath = fullpath + "/" + name.hash() + time
    set(ref(database, getpath), {
      name,
      text,
      time: time,
    });
  }

  function checkType(obj, type) {
    return obj && typeof (obj) === type
  }

  // Fill with comments
  get(child(ref(database), fullpath)).then((snapshot) => {
    if (snapshot.exists()) {
      const comments = snapshot.val();
      for (const cid of Object.keys(comments)) {
        const comment = comments[cid]
        if (checkType(comment.name, "string") && checkType(comment.text, "string") && checkType(comment.time, "number"))
          addCommentHtml(comment)
      }
    }
    document.getElementById("comments").style.display = "block"
  }).catch((error) => {
    console.error(error);
    document.getElementById("comments").style.display = "block"
  })


  function addCommentHtml(commentObj) {
    document.getElementById("nocomment").style.display = "none"

    var li = document.createElement("li");
    const div = document.createElement("div")
    div.classList.add("comment")

    const name = document.createTextNode(commentObj.name);
    const nameSpan = document.createElement("span")
    nameSpan.append(name)

    const comment = document.createTextNode(commentObj.text);
    const commentp = document.createElement("p")
    commentp.append(comment)

    const utctime = commentObj.time
    const timestamp_text = "in " + new Date(utctime).toLocaleDateString() + " " + new Date(utctime).toLocaleTimeString()
    const time = document.createTextNode(timestamp_text);
    const timestamp = document.createElement("timestamp")
    timestamp.append(time)

    div.appendChild(nameSpan);
    div.appendChild(timestamp);
    div.appendChild(commentp);
    li.appendChild(div)
    document.getElementById("comment-list").prepend(li);
  }

  function processUserCommentAdd() {
    const name = document.getElementById("comment-name");
    const text = document.getElementById("comment-box");
    addCommentHtml({ name: name.value, text: text.value, time: Date.now() })
    postComment(name.value, text.value)
    setCookie('username', name.value);
    name.disabled = true
    text.value = ""
  }

  const form = document.getElementById("new-comment-form");
  form.addEventListener("submit", function(e) {
    e.preventDefault()
    processUserCommentAdd()
  });

  document.body.addEventListener('keydown', function(e) {
    if (e.repeat) return;
    if (!(e.key === 'Enter' && (e.metaKey || e.ctrlKey))) return;
    const target = e.target
    if (target.form) {
      e.preventDefault()
      if (form.checkValidity() === false) {
        form.reportValidity();
        return;
      }
      processUserCommentAdd()
    }
  })
}

setupComments();
