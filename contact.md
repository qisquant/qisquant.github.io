---
layout: page
title: Contact
permalink: contact
---

  <form action="//formspree.io/sysresearcher@gmail.com" role="form" method="POST">
    <div class="form-group">
      <label for="name">What's your name?</label>
      <input type="text" name="name" class="form-control" required>
    </div>

    <div class="form-group">
      <label for="_replyto">What's your email address?</label>
      <input type="email" name="_replyto" class="form-control" required>
    </div>

    <div class="form-group">
      <label for="message">How can I help you?</label>
      <textarea name="body" class="form-control" rows="4" width="100%" required></textarea>
    </div>

    <div>
      <input type="hidden" name="_next" value="//qisresearch.github.io/thankyou" />
      <input type="hidden" name="_subject" value="New submission!" />
      <input type="submit" class="btn btn-lg" value="Let's talk">
      or <a href="mailto:sysresearcher@gmail.com">drop a simple email</a>
    </div>
  </form>
