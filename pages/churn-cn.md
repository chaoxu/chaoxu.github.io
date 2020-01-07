---
title: 薅羊毛日记
---


 <script type="text/javascript">
  function AdjustIFrame(id) {
      var frame = document.getElementById(id);
      var maxW = frame.scrollWidth;
      var minW = maxW;
      var FrameH = 100; //IFrame starting height
      frame.style.height = FrameH + "px"

      while (minW == maxW and minW <= 40000) {
          FrameH = FrameH * 2; //Increment
          frame.style.height = FrameH + "px";
          minW = frame.scrollWidth;
      }
  }

 </script>

<iframe id="RefFrame" onload="AdjustIFrame('RefFrame');" class="RefFrame"
    src="https://dynalist.io/d/GObI1Y0XwI4CYq-UL3kzR4D-"></iframe>