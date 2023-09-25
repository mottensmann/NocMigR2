#import soundfile as sf
from pydub import AudioSegment as am
import wave
import audioop
def resample_wave_mono(in_file, out_file, resample_rate):
  ## Step 1: Coerce to mono
    try:
      stereo = wave.open(in_file, 'rb')
      mono = wave.open(out_file, 'wb')
      mono.setparams(stereo.getparams())
      mono.setnchannels(1)
      channels = stereo.getnchannels()
      frame_rate = stereo.getframerate()
      if channels == 2:
        # TODO: write code...
        mono.writeframes(audioop.tomono(stereo.readframes(float('inf')), stereo.getsampwidth(), 1, 1))
      else:
        mono.writeframes(stereo.readframes(float('inf'))) 
      mono.close()
      stereo.close()
    except:
      return False
  ## Step 2: Resample
    try:
      ## read soundfile
      sound = am.from_file(out_file, format='wav', frame_rate=frame_rate)
      sound = sound.set_frame_rate(int(resample_rate))
      sound.export(out_file, format='wav')
      # data, samplerate = sf.read(out_file)
      # sf.write(out_file, data,int(resample_rate))
    except:
      return False
    return True
  
def resample_wave_stereo(in_file, out_file, resample_rate):
    try:
      ## read soundfile
      sound = am.from_file(out_file, format='wav', frame_rate=frame_rate)
      sound = sound.set_frame_rate(int(resample_rate))
      sound.export(out_file, format='wav')
      #data, samplerate = sf.read(in_file)
      #sf.write(out_file, data,int(resample_rate))
    except:
      return False
    return True
