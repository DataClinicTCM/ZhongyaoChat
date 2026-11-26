/* ==========================================================================
   chat_export.js — Chat 页「导出对话」

   在浏览器端读取 shinychat 已经渲染出来的消息，按「我 / 助手」整理成纯文本下载。
   放在前端做的好处：服务端不用另存一份对话；流式回答、检索式回答、
   正在生成中的回答，导出的都是屏幕上实际显示的内容。
   ========================================================================== */

(function () {
  "use strict";

  function pad(n) {
    return (n < 10 ? "0" : "") + n;
  }

  function stamp(d) {
    return String(d.getFullYear()) + pad(d.getMonth() + 1) + pad(d.getDate()) +
      "_" + pad(d.getHours()) + pad(d.getMinutes());
  }

  // 单条消息的正文：去掉复制按钮、脚注回跳箭头等控件，再压缩多余空行。
  // 注意：innerText 只对「已渲染」的元素生效，所以副本要先临时挂到页面上
  // （放到屏幕外），否则会退化成 textContent，段落与表格换行都会丢。
  function messageText(msg) {
    var body = msg.querySelector(".shiny-chat-message-content") || msg;
    var clone = body.cloneNode(true);
    Array.prototype.forEach.call(
      clone.querySelectorAll("button, .code-copy-button, .copy-button, [data-footnote-backref]"),
      function (el) { el.remove(); }
    );
    var holder = document.createElement("div");
    holder.setAttribute("aria-hidden", "true");
    holder.style.cssText = "position:absolute;left:-9999px;top:0;width:820px;";
    holder.appendChild(clone);
    document.body.appendChild(holder);
    var text = clone.innerText || clone.textContent || "";
    holder.remove();
    return text
      .replace(/\r\n/g, "\n")
      .replace(/[ \t]+\n/g, "\n")
      .replace(/\n{3,}/g, "\n\n")
      .trim();
  }

  // 整个对话（按 DOM 顺序）：用户消息是 .shiny-chat-user-message，助手是 .shiny-chat-message
  function transcript() {
    var scope = document.querySelector("#chat_wrapper") || document;
    var msgs = scope.querySelectorAll(".shiny-chat-message, .shiny-chat-user-message");
    var parts = [];
    Array.prototype.forEach.call(msgs, function (m) {
      var text = messageText(m);
      if (!text) return;
      var who = m.classList.contains("shiny-chat-user-message") ? "我" : "助手";
      parts.push("【" + who + "】\n" + text);
    });
    return parts.join("\n\n");
  }

  window.zhChatTranscript = transcript;

  window.zhDownloadChat = function () {
    var body = transcript();
    if (!body) {
      window.alert("对话还是空的，先提一个问题再导出。");
      return;
    }
    var now = new Date();
    var head = [
      "ZhongyaoChat · 《中华药海》知识图谱对话",
      "导出时间：" + now.toLocaleString("zh-CN"),
      "说明：按对话顺序导出，【我】为提问，【助手】为回答；回答里的 [E1]、[E2] 等标号对应右侧「检索链路」面板里的证据出处。",
      "",
      "------------------------------------------------------------",
      ""
    ].join("\n");
    var blob = new Blob([head + body + "\n"], { type: "text/plain;charset=utf-8" });
    var url = URL.createObjectURL(blob);
    var a = document.createElement("a");
    a.href = url;
    a.download = "中华药海对话_" + stamp(now) + ".txt";
    document.body.appendChild(a);
    a.click();
    setTimeout(function () {
      URL.revokeObjectURL(url);
      a.remove();
    }, 1500);
  };
})();
