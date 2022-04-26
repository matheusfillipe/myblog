// Import the functions you need from the SDKs you need
//
import { initializeApp } from "https://www.gstatic.com/firebasejs/9.6.11/firebase-app.js";
import { getDatabase, ref, set, get, child } from "https://www.gstatic.com/firebasejs/9.6.11/firebase-database.js";

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

// Page path
const path = window.location.pathname.toString().split("/").slice(0, -1).join("/")

// DB functions
function postComment(name, text) {
  const time = Date.now()
  const refPath = path + "/" + name + time
  set(ref(database, refPath), {
    name,
    text,
    time: time
  });
}

function checkType(obj, type) {
  return obj && typeof (obj) === type
}

// Fill with comments
get(child(ref(database), path)).then((snapshot) => {
  if (snapshot.exists()) {
    const comments = snapshot.val();
    for (const cid of Object.keys(comments)) {
      const comment = comments[cid]
      if (checkType(comment.name, "string") && checkType(comment.text, "string") && checkType(comment.time, "number"))
        addComment(comment)
    }
  }
}).catch((error) => {
  console.error(error);
});


function addComment(commentObj) {
  document.getElementById("nocomment").style.display = "none"

  var li = document.createElement("li");
  const name = document.createTextNode(commentObj.name);
  const comment = document.createTextNode(commentObj.text);

  const utctime = commentObj.time
  const timestamp = "at " + new Date(utctime).toLocaleDateString() + " " + new Date(utctime).toLocaleTimeString()
  const time = document.createTextNode(timestamp);
  // TODO style this

  li.appendChild(name);
  li.appendChild(comment);
  li.appendChild(time);
  document.getElementById("comment-list").prepend(li);
}

function processCommentAdd() {
  const name = document.getElementById("comment-name");
  const text = document.getElementById("comment-box");
  addComment({ name: name.value, text: text.value, time: Date.now() })
  postComment(name.value, text.value)
  name.value = ""
  text.value = ""
}

const form = document.getElementById("new-comment-form");
form.addEventListener("submit", function(e) {
  e.preventDefault()
  processCommentAdd()
});

document.body.addEventListener('keydown', function(e) {
  if (!(e.key === 'Enter' && (e.metaKey || e.ctrlKey))) return;
  const target = e.target
  if (target.form) {
    e.preventDefault()
    if (form.checkValidity() === false){
        form.reportValidity();
        return;
    }
    processCommentAdd()
  }
})
