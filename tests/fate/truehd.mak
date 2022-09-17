FATE_TRUEHD-$(call DEMDEC, TRUEHD, TRUEHD) += fate-truehd-5.1
fate-truehd-5.1: CMD = md5pipe -f truehd -i $(TARGET_SAMPLES)/lossless-audio/truehd_5.1.raw -f s32le
fate-truehd-5.1: CMP = oneline
fate-truehd-5.1: REF = 95d8aac39dd9f0d7fb83dc7b6f88df35

FATE_TRUEHD-$(call DEMDEC, TRUEHD, TRUEHD) += fate-truehd-5.1-downmix-2.0
fate-truehd-5.1-downmix-2.0: CMD = md5pipe -f truehd -request_channel_layout FL+FR -i $(TARGET_SAMPLES)/lossless-audio/truehd_5.1.raw -f s32le
fate-truehd-5.1-downmix-2.0: CMP = oneline
fate-truehd-5.1-downmix-2.0: REF = a269aee0051d4400c9117136f08c9767

FATE_TRUEHD-$(call ALLYES, TRUEHD_DEMUXER TRUEHD_MUXER TRUEHD_CORE_BSF) += fate-truehd-core-bsf
fate-truehd-core-bsf: CMD = md5pipe -i $(TARGET_SAMPLES)/truehd/atmos.thd -c:a copy -bsf:a truehd_core -fflags +bitexact -f truehd
fate-truehd-core-bsf: CMP = oneline
fate-truehd-core-bsf: REF = 3aa5d0c7825051f3657b71fd6135183b

FATE_TRUEHD_ENCODE-$(call ENCDEC2, PCM_S16LE, TRUEHD, TRUEHD, WAV_DEMUXER PCM_S16LE_MUXER ARESAMPLE_FILTER) += fate-truehd-encode-s16-stereo
fate-truehd-encode-s16-stereo: CMD = enc_dec \
        wav $(TARGET_SAMPLES)/audio-reference/chorusnoise_2ch_44kHz_s16.wav \
        truehd "-c truehd -strict experimental" "s16le" "-af aresample" "" \
        "-show_entries stream=codec_name,sample_rate,channel_layout,bits_per_raw_sample"

FATE_TRUEHD_ENCODE-$(call ENCDEC2, PCM_S24LE, TRUEHD, TRUEHD, WAV_DEMUXER PCM_S24LE_MUXER) += fate-truehd-encode-s24-stereo
fate-truehd-encode-s24-stereo: CMD = enc_dec \
        wav $(TARGET_SAMPLES)/audio-reference/divertimenti_2ch_96kHz_s24.wav \
        truehd "-c truehd -strict experimental" "s24le" "" "" \
        "-show_entries stream=codec_name,sample_rate,channel_layout,bits_per_raw_sample"

FATE_TRUEHD_ENCODE-$(call ENCDEC2, PCM_S16LE, TRUEHD, TRUEHD, WAV_DEMUXER PCM_S16LE_MUXER ARESAMPLE_FILTER) += fate-truehd-encode-s16-5.1
fate-truehd-encode-s16-5.1: CMD = enc_dec \
        wav $(TARGET_SAMPLES)/audio-reference/yo.raw-short.wav \
        truehd "-c truehd -strict experimental" "s16le" "-af aresample" "" \
        "-show_entries stream=codec_name,sample_rate,channel_layout,bits_per_raw_sample"

FATE_MLP_ENCODE-$(call ENCDEC2, PCM_S16LE, MLP, MLP, WAV_DEMUXER PCM_S16LE_MUXER) += fate-mlp-encode-s16-mono
fate-mlp-encode-s16-mono: CMD = enc_dec \
        wav $(TARGET_SAMPLES)/audiomatch/tones_44100_mono.wav \
        mlp "-c mlp -strict experimental" "s16le" "" "" \
        "-show_entries stream=codec_name,sample_rate,channel_layout,bits_per_raw_sample"

FATE_MLP_ENCODE-$(call ENCDEC2, PCM_S16LE, MLP, MLP, WAV_DEMUXER PCM_S16LE_MUXER) += fate-mlp-encode-s16-stereo
fate-mlp-encode-s16-stereo: CMD = enc_dec \
        wav $(TARGET_SAMPLES)/audio-reference/chorusnoise_2ch_44kHz_s16.wav \
        mlp "-c mlp -strict experimental" "s16le" "" "" \
        "-show_entries stream=codec_name,sample_rate,channel_layout,bits_per_raw_sample"

FATE_MLP_ENCODE-$(call ENCDEC2, PCM_S24LE, MLP, MLP, WAV_DEMUXER PCM_S24LE_MUXER) += fate-mlp-encode-s24-stereo
fate-mlp-encode-s24-stereo: CMD = enc_dec \
        wav $(TARGET_SAMPLES)/audio-reference/divertimenti_2ch_96kHz_s24.wav \
        mlp "-c mlp -strict experimental" "s24le" "" "" \
        "-show_entries stream=codec_name,sample_rate,channel_layout,bits_per_raw_sample"

FATE_MLP_ENCODE-$(call ENCDEC2, PCM_S16LE, MLP, MLP, PCM_S16LE_DEMUXER PCM_S16LE_MUXER) += fate-mlp-encode-s16-3.0
fate-mlp-encode-s16-3.0: CMD = enc_dec \
        "s16le -ac 3 -ar 44100" $(TARGET_SAMPLES)/aac/al06_44_reorder.s16 \
        mlp "-c mlp -strict experimental" "s16le" "" "" \
        "-show_entries stream=codec_name,sample_rate,channel_layout,bits_per_raw_sample"

FATE_MLP_ENCODE-$(call ENCDEC2, PCM_S16LE, MLP, MLP, PCM_S16LE_DEMUXER PCM_S16LE_MUXER) += fate-mlp-encode-s16-4.0
fate-mlp-encode-s16-4.0: CMD = enc_dec \
        "s16le -ac 4 -ar 48000" $(TARGET_SAMPLES)/ac3/millers_crossing_4.0_v2.pcm \
        mlp "-c mlp -strict experimental" "s16le" "" "" \
        "-show_entries stream=codec_name,sample_rate,channel_layout,bits_per_raw_sample"

FATE_MLP_ENCODE-$(call ENCDEC2, PCM_S16LE, MLP, MLP, WAV_DEMUXER PCM_S16LE_MUXER) += fate-mlp-encode-s16-5.1
fate-mlp-encode-s16-5.1: CMD = enc_dec \
        wav $(TARGET_SAMPLES)/audio-reference/yo.raw-short.wav \
        mlp "-c mlp -strict experimental" "s16le" "" "" \
        "-show_entries stream=codec_name,sample_rate,channel_layout,bits_per_raw_sample"

fate-mlp-encode-s16% fate-truehd-encode-s16%: CMP_UNIT = 2
fate-mlp-encode-s24% fate-truehd-encode-s24%: CMP_UNIT = 3

FATE_SAMPLES_AUDIO += $(FATE_TRUEHD-yes)
FATE_SAMPLES_FFMPEG_FFPROBE += $(FATE_TRUEHD_ENCODE-yes) $(FATE_MLP_ENCODE-yes)
fate-truehd: $(FATE_TRUEHD-yes) $(FATE_TRUEHD_ENCODE-yes) $(FATE_MLP_ENCODE-yes)
